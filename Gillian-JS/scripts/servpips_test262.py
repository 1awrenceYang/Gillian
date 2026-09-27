#!/usr/bin/env python3
"""SERVPIPS V1: the ES5 test262 subset in upstream, exec --servpips and
wpst --servpips modes.

The test suite is GillianPlatform/javert-test262 at the revision used by the
upstream CI (.github/workflows/ci.yml, job `test262`). It is fetched by the
`fetch` subcommand and never committed.

Test selection, strictness and expectations follow Gillian-JS/lib/Test262
exactly:
  * a file is selected when it ends in .js and its path contains none of the
    substrings of Test262_filtering.non_covered_tests (read from the OCaml
    source; with --ci also failing_tests);
  * [noStrict] files are not run (the upstream runner skips the NonStrict
    category), [onlyStrict] and unflagged files run once in strict mode
    ("use strict"; + test262 harness + test), [raw] files run as they are;
  * positive tests must finish normally; negative parse/early tests must fail
    with a JS parser error ("Parsing error", not an EarlyError); negative
    runtime tests must end in error mode (only the bulk mode also checks the
    class of the thrown value, which the exec/wpst CLIs do not report).

Modes (one run of gillian-js per test, in long-running containers of the
engine image, --jobs of them in parallel):
  bulk           gillian-js test262 <test file> (the upstream runner itself on
                 one file: Gillian's own verdict, including the class of the
                 thrown value for negative runtime tests)
  exec           gillian-js exec <test>                 (upstream semantics)
  exec-servpips  gillian-js exec --servpips <test>
  wpst-servpips  gillian-js wpst <test> --servpips --servpips-log ... -l disabled
                 --unroll 2000 --smt-timeout 5000   (the flags of run.js; all
                 inputs are concrete, so a conforming run has exactly one
                 `end` event)
  wpst           gillian-js wpst <test> --unroll 2000 with SERVPIPS_FULL_INIT=1
                 (the fork's symbolic engine without SERVPIPS mode, full ES5
                 initial heap); a diagnostic mode, normally run with
                 --tests-file on the tests that fail in wpst-servpips only

In wpst mode (Cosette compilation) an uncaught exception in global code is
reported as end{error, "main:<n>: Pure assertion failed: false"} (the global
error assertion of the compiled main); it is the wpst counterpart of error
mode. A crash of the Cosette pre-parser (JS_PreParser.Unparseable, whose
message also says "Parsing error") is its own class, preparser-crash, and
never counts as the parse error of a negative test.

Another image (e.g. one built from upstream master) is run with
`run --image I --label L`; its results go to O/<mode>@L.jsonl and the report
compares bulk@upstream / exec@upstream with bulk / exec.

Subcommands:
  fetch   clone the suite:          fetch --dir D
  run     run one mode:             run --suite D --out O --mode M [--image I]
                                    [--label L] [--tests-file F [--redo]]
  report  tables and diff lists:    report --out O
  all     fetch (if needed); exec, bulk, exec-servpips, wpst-servpips; wpst
          on the tests that pass under exec and fail under wpst-servpips;
          report

Timeouts: 180 s per test (900 s in wpst mode); `all` runs exec first and
skips in the other modes the tests that time out under exec (they cannot be
regressions); the other modes run the slowest exec tests first.

Results: O/<mode>.jsonl (one JSON object per test, resumable), O/report.md,
O/verdicts.tsv.
"""

import argparse
import collections
import concurrent.futures
import json
import os
import re
import shutil
import subprocess
import sys
import threading
import time

TEST262_REPO = "https://github.com/GillianPlatform/javert-test262"
TEST262_REV = "93e0d0b04093cabc3234a776eec5cc3e165f3b1a"  # upstream CI pin
HARNESS_IN_IMAGE = "/opt/gillian/share/gillian-js/runtime/harness.js"
WPST_ARGS = ["--unroll", "2000", "--smt-timeout", "5000"]  # as run.js
MODES = ["bulk", "exec", "exec-servpips", "wpst-servpips", "wpst"]
HERE = os.path.dirname(os.path.abspath(__file__))
FILTERING_ML = os.path.join(HERE, "..", "lib", "Test262", "Test262_filtering.ml")


def log(msg):
    print(msg, file=sys.stderr, flush=True)


def default_image():
    try:
        sha = subprocess.run(["git", "-C", HERE, "rev-parse", "HEAD"],
                             capture_output=True, text=True, check=True).stdout.strip()
        return "servpips-gillian:" + sha[:12]
    except Exception:
        return "servpips-gillian:latest"


# ---------------------------------------------------------------------------
# Selection (mirrors Test262_suite.ml / Test262_filtering.ml)

def ocaml_string_lists(path):
    """name -> list of string literals of each top-level `let name = [ ... ]`."""
    src = open(path, encoding="utf-8").read()
    out = {}
    for m in re.finditer(r"^let\s+(\w+)\s*=\s*\[(.*?)\]", src, re.S | re.M):
        out[m.group(1)] = re.findall(r'"((?:[^"\\]|\\.)*)"', m.group(2))
    return out


def filter_lists(ci):
    lists = ocaml_string_lists(FILTERING_ML)
    non_covered = (lists["non_strict_tests"] + lists["tests_for_unimplemented_features"]
                   + lists["tests_using_unimplemented_features"] + lists["es6_tests"])
    failing = lists["failing_tests"]
    return non_covered + (failing if ci else []), failing


def test_info(code):
    """(run mode, negative) as Test262_suite.create_tests; run mode is None for
    [noStrict] files (not run upstream)."""
    if b"[noStrict]" in code:
        rm = None
    elif b"[onlyStrict]" in code:
        rm = "strict"
    elif b"[raw]" in code:
        rm = "raw"
    else:
        rm = "strict"
    neg = None
    if b"negative:" in code:
        # Str.search_forward on the whole file: first match, `$` = end of line
        ph = re.search(rb"phase:[ \t]*(.*?)[ \t]*$", code, re.M)
        ty = re.search(rb"type:[ \t]*(.*?)[ \t]*$", code, re.M)
        neg = {"phase": ph.group(1).decode() if ph else None,
               "type": ty.group(1).decode() if ty else None}
    return rm, neg


def select_tests(suite, ci=False, only=None):
    """[(rel, run_mode, neg)] for the selected files, plus counts."""
    filt, _ = filter_lists(ci)
    root = os.path.join(suite, "test")
    tests, counts = [], collections.Counter()
    for sub in ("built-ins", "language"):
        for d, _, files in os.walk(os.path.join(root, sub)):
            for f in files:
                rel = os.path.relpath(os.path.join(d, f), root)
                if not f.endswith(".js"):
                    continue
                counts["js files"] += 1
                if any(s in rel for s in filt):
                    counts["filtered out"] += 1
                    continue
                if only and not any(s in rel for s in only):
                    continue
                code = open(os.path.join(d, f), "rb").read()
                rm, neg = test_info(code)
                if rm is None:
                    counts["noStrict (not run)"] += 1
                    continue
                counts["selected"] += 1
                tests.append((rel, rm, neg))
    tests.sort()
    return tests, counts


def build_source(suite, rel, rm, harness):
    code = open(os.path.join(suite, "test", rel), "rb").read()
    if rm == "raw":
        return code
    # Io_utils.load_js_file: use_strict ^ harness ^ "\n\n" ^ file
    return b'"use strict";\n\n' + harness + b"\n\n" + code


# ---------------------------------------------------------------------------
# Containers

class Pool:
    def __init__(self, image, tag, mounts, jobs, memory):
        self.image, self.names = image, []
        uid, gid = os.getuid(), os.getgid()
        for i in range(jobs):
            name = "v1-%s-%d-%d" % (tag, os.getpid(), i)
            cmd = ["docker", "run", "-d", "--rm", "--init", "--name", name,
                   "--user", "%d:%d" % (uid, gid), "--network", "none",
                   "--memory", memory, "--entrypoint", "sleep"]
            for m in mounts:
                cmd += ["-v", "%s:%s" % (m, m)]
            cmd += [image, "infinity"]
            subprocess.run(cmd, check=True, capture_output=True)
            self.names.append(name)

    def close(self):
        if self.names:
            subprocess.run(["docker", "rm", "-f"] + self.names, capture_output=True)
            self.names = []


def image_harness(image):
    return subprocess.run(["docker", "run", "--rm", "--entrypoint", "cat", image,
                           HARNESS_IN_IMAGE], capture_output=True, check=True).stdout


def image_commit(image):
    r = subprocess.run(["docker", "image", "inspect", image, "--format",
                        '{{index .Config.Labels "servpips.fork.commit"}}'],
                       capture_output=True, text=True)
    return r.stdout.strip()


# ---------------------------------------------------------------------------
# One test

def tail(b, n=600):
    s = b.decode("utf-8", "replace") if isinstance(b, bytes) else b
    s = s.replace("\r", "")
    return s if len(s) <= n else "..." + s[-n:]


def parse_events(path, max_ends=200):
    ends, stats, n_ends = [], None, 0
    status = collections.Counter()
    try:
        with open(path, "rb") as fh:
            for line in fh:
                if line.startswith(b'{"ev":"hello"'):
                    continue
                try:
                    ev = json.loads(line)
                except Exception:
                    continue
                if ev.get("ev") == "end":
                    n_ends += 1
                    status[ev.get("status")] += 1
                    if len(ends) < max_ends:
                        ends.append({"status": ev.get("status"),
                                     "reason": (ev.get("reason") or "")[:300]})
                elif ev.get("ev") == "stats":
                    stats = {k: ev.get(k) for k in ("leaves", "infeasible", "vanished",
                                                    "prunes", "max_branch", "fatal",
                                                    "seconds", "rss_mb")}
                    stats["solver"] = ev.get("solver")
    except FileNotFoundError:
        pass
    return n_ends, dict(status), ends, stats


UNCAUGHT_RE = re.compile(r"^main:\d+: Pure assertion failed: false$")


def classify_exec(rc, out, neg, killed):
    """(verdict, class, detail)"""
    text = out.decode("utf-8", "replace")
    if killed:
        return "fail", "timeout", ""
    if rc == 0:
        got = "normal"
    elif rc == 1:
        got = "error-mode"
    elif rc == 2:
        got = "parse-error" if "Parsing error" in text else (
            "early-error" if "EarlyError" in text else "compile-error")
    elif rc == 125:
        got = "internal-error"
    else:
        got = "rc=%d" % rc
    if neg is None:
        ok = got == "normal"
    elif neg["phase"] in ("parse", "early"):
        ok = got == "parse-error"
    elif neg["phase"] == "runtime":
        ok = got == "error-mode"
    else:
        ok = False
    return ("pass" if ok else "fail"), got, ("" if ok else tail(out, 400))


def classify_wpst(rc, out, neg, killed, n_ends, status, ends, stats):
    if killed:
        return "fail", "timeout", ""
    fatal = (stats or {}).get("fatal")
    if stats is None:
        got, detail = ("no-stats(rc=%d)" % rc), tail(out, 400)
    elif fatal:
        if "Unparseable" in fatal:
            # JS_PreParser (Cosette Assert/Assume stringifier) crashed; its
            # messages also say "Parsing error"
            got = "preparser-crash"
        elif "Parsing error" in fatal:
            got = "parse-error"
        elif "EarlyError" in fatal:
            got = "early-error"
        elif "compilation" in fatal.lower():
            got = "compile-error"
        else:
            got = "fatal"
        detail = fatal[:400]
    elif n_ends == 0:
        got, detail = "no-end", json.dumps(stats)[:400]
    elif n_ends > 1:
        got = "multi-path(%d:%s)" % (n_ends, ",".join(
            "%s=%d" % kv for kv in sorted(status.items())))
        reasons = collections.Counter("%s: %s" % (e["status"], e["reason"][:120]) for e in ends)
        detail = "; ".join("%dx %s" % (c, r) for r, c in reasons.most_common(4))
    else:
        e = ends[0]
        if e["status"] == "returned":
            got = "normal"
        elif e["status"] == "error" and UNCAUGHT_RE.match(e["reason"] or ""):
            got = "error-mode"
        elif e["status"] == "threw":
            got = "error-mode"
        else:
            got = e["status"]
        detail = e["reason"]
    if neg is None:
        ok = got == "normal"
    elif neg["phase"] in ("parse", "early"):
        ok = got == "parse-error"
    elif neg["phase"] == "runtime":
        ok = got == "error-mode"
    else:
        ok = False
    return ("pass" if ok else "fail"), got, ("" if ok else detail)


BULK_RE = re.compile(r"\[(OK|FAIL|ERROR|SKIP)\]\s+\S+\s+\d+\s+\S+")


def classify_bulk(rc, out, killed):
    text = out.decode("utf-8", "replace").replace("\r", "\n")
    if killed:
        return "fail", "timeout", ""
    st = [m.group(1) for m in BULK_RE.finditer(text)]
    if len(st) != 1:
        return "fail", "no-result(rc=%d,%d)" % (rc, len(st)), tail(out, 400)
    if st[0] == "OK":
        return "pass", "OK", ""
    m = re.search(r"Test error: (.*?)(?:\nASSERT|\Z)", text, re.S)
    return "fail", st[0], (m.group(1).strip() if m else tail(out, 400))[:400]


def classify_wpst_upstream(rc, out, neg, killed):
    text = out.decode("utf-8", "replace")
    if killed:
        return "fail", "timeout", ""
    detail = ""
    if rc == 0 and "Success!" in text:
        got = "normal"
    elif rc == 1 and "Errors occurred!" in text:
        errs = re.findall(r"FAILURE TERMINATION: Procedure (\S+), Command \d+.*?\n\s*Errors: ([^\n]*)",
                          text, re.S)
        if errs and all(p == "main" and e.strip() == "Pure assertion failed: false"
                        for p, e in errs):
            got = "error-mode" if len(errs) == 1 else "multi-path(%d errors)" % len(errs)
            detail = "%d x main: Pure assertion failed: false" % len(errs)
        else:
            got = "error"
            detail = "; ".join("%s: %s" % (p, e.strip()[:150]) for p, e in errs[:3]) or tail(out, 400)
    elif rc == 2:
        got = "parse-error" if "Parsing error" in text else (
            "early-error" if "EarlyError" in text else "compile-error")
    elif rc == 125:
        got = "internal-error"
    else:
        got = "rc=%d" % rc
    if neg is None:
        ok = got == "normal"
    elif neg["phase"] in ("parse", "early"):
        ok = got == "parse-error"
    elif neg["phase"] == "runtime":
        ok = got == "error-mode"
    else:
        ok = False
    return ("pass" if ok else "fail"), got, ("" if ok else (detail or tail(out, 400)))


def run_one(pool_name, mode, src, work, neg, timeout):
    os.makedirs(work, exist_ok=True)
    ev = os.path.join(work, "events.jsonl")
    if os.path.exists(ev):
        os.remove(ev)
    if mode == "bulk":
        args = ["test262", src, "-l", "disabled"]
    elif mode == "exec":
        args = ["exec", src, "-l", "disabled", "--result-dir", ".gillian"]
    elif mode == "exec-servpips":
        args = ["exec", "--servpips", src, "-l", "disabled", "--result-dir", ".gillian"]
    elif mode == "wpst-servpips":
        args = (["wpst", src, "--servpips", "--servpips-log", ev, "-l", "disabled",
                 "--result-dir", ".gillian"] + WPST_ARGS)
    elif mode == "wpst":
        args = ["wpst", src, "-l", "disabled", "--result-dir", ".gillian", "--unroll", "2000"]
    else:
        raise ValueError(mode)
    env = ["-e", "SERVPIPS_FULL_INIT=1"] if mode == "wpst" else []
    cmd = (["docker", "exec", "-w", work] + env + [pool_name, "timeout", "-s", "KILL",
            str(timeout), "gillian-js"] + args)
    t0 = time.time()
    p = subprocess.run(cmd, capture_output=True)
    secs = time.time() - t0
    out = p.stdout + p.stderr
    killed = p.returncode == 137 or p.returncode == -9
    rec = {"rc": p.returncode, "secs": round(secs, 3)}
    if mode == "wpst":
        v, c, d = classify_wpst_upstream(p.returncode, out, neg, killed)
    elif mode == "wpst-servpips":
        n_ends, status, ends, stats = parse_events(ev)
        v, c, d = classify_wpst(p.returncode, out, neg, killed, n_ends, status, ends, stats)
        rec.update({"n_ends": n_ends, "ends": status, "stats": stats})
        if n_ends > 1:
            rec["end_sample"] = ends[:20]
    elif mode == "bulk":
        v, c, d = classify_bulk(p.returncode, out, killed)
        shutil.rmtree(os.path.join(work, "_build"), ignore_errors=True)
    else:
        v, c, d = classify_exec(p.returncode, out, neg, killed)
    rec.update({"verdict": v, "class": c, "detail": d})
    return rec


def cmd_run(a):
    suite, out = os.path.abspath(a.suite), os.path.abspath(a.out)
    os.makedirs(out, exist_ok=True)
    tests, counts = select_tests(suite, a.ci, a.only)
    log("selection: %s" % dict(counts))
    harness = image_harness(a.image)
    srcdir = os.path.join(out, "src")
    key = a.mode + ("@" + a.label if a.label else "")
    res_path = os.path.join(out, key + ".jsonl")
    done = set()
    if a.redo and a.tests_file and os.path.exists(res_path):
        # drop the earlier records of the listed tests, they are run again
        wanted = set(l.strip() for l in open(a.tests_file) if l.strip())
        keep = [l for l in open(res_path) if json.loads(l)["test"] not in wanted]
        open(res_path, "w").writelines(keep)
    if os.path.exists(res_path) and not a.fresh:
        for line in open(res_path):
            try:
                done.add(json.loads(line)["test"])
            except Exception:
                pass
    elif os.path.exists(res_path):
        os.remove(res_path)
    todo = [t for t in tests if t[0] not in done]
    if a.tests_file:
        wanted = set(l.strip() for l in open(a.tests_file) if l.strip())
        todo = [t for t in todo if t[0] in wanted]
    ref = load(out, a.ref) if a.ref else {}
    skipped = []
    if a.ref and a.skip_ref_timeouts:
        # a test that times out under the reference mode (normally exec, the
        # upstream semantics) cannot be a regression; it is recorded as skipped
        skipped = [t for t in todo if ref.get(t[0], {}).get("class") == "timeout"]
        todo = [t for t in todo if ref.get(t[0], {}).get("class") != "timeout"]
    if ref:
        # slowest first (by the reference run), so that no straggler is left
        todo.sort(key=lambda t: -ref.get(t[0], {}).get("secs", 0))
    if a.limit:
        todo = todo[:a.limit]
    log("%s: %d selected, %d already done, %d to run, image %s (%s)" % (
        key, len(tests), len(done), len(todo), a.image, image_commit(a.image)))
    if skipped:
        with open(res_path, "a") as fh:
            for rel, rm, neg in skipped:
                fh.write(json.dumps({"test": rel, "run": rm, "neg": neg, "mode": key,
                                     "image": a.image, "verdict": "skip",
                                     "class": "skipped(%s timeout)" % a.ref,
                                     "detail": ""}) + "\n")
        log("%s: %d tests skipped (timeout under %s)" % (a.mode, len(skipped), a.ref))
    for rel, rm, neg in (todo if a.mode != "bulk" else []):
        dst = os.path.join(srcdir, rel)
        if not os.path.exists(dst):
            os.makedirs(os.path.dirname(dst), exist_ok=True)
            with open(dst, "wb") as fh:
                fh.write(build_source(suite, rel, rm, harness))
    if not todo:
        return
    pool = Pool(a.image, key.replace("@", "-"), [out, suite], a.jobs, a.memory)
    lock = threading.Lock()
    fh = open(res_path, "a")
    free = list(pool.names)
    t_start, n_done = time.time(), [0]
    stats = collections.Counter()

    def job(item):
        rel, rm, neg = item
        with lock:
            name = free.pop()
        try:
            work = os.path.join(out, "work", key, name)
            timeout = a.timeout or (900 if a.mode.startswith("wpst") else 180)
            src = (os.path.join(suite, "test", rel) if a.mode == "bulk"
                   else os.path.join(srcdir, rel))
            rec = run_one(name, a.mode, src, work, neg, timeout)
        finally:
            with lock:
                free.append(name)
        rec = dict({"test": rel, "run": rm, "neg": neg, "mode": key,
                    "image": a.image}, **rec)
        with lock:
            fh.write(json.dumps(rec) + "\n")
            fh.flush()
            n_done[0] += 1
            stats[rec["verdict"]] += 1
            if n_done[0] % 200 == 0 or n_done[0] == len(todo):
                log("%s: %d/%d  %s  %.0fs" % (key, n_done[0], len(todo), dict(stats),
                                              time.time() - t_start))

    try:
        with concurrent.futures.ThreadPoolExecutor(a.jobs) as ex:
            list(ex.map(job, todo))
    finally:
        fh.close()
        pool.close()


# ---------------------------------------------------------------------------
# Report

def load(out, mode):
    p = os.path.join(out, mode + ".jsonl")
    res = {}
    if os.path.exists(p):
        for line in open(p):
            try:
                r = json.loads(line)
            except Exception:
                continue
            res[r["test"]] = r
    return res


def group_of(rel):
    parts = rel.split("/")
    return "/".join(parts[:2])


def cmd_report(a):
    out = os.path.abspath(a.out)
    found = [f[:-6] for f in os.listdir(out) if f.endswith(".jsonl")]
    order = ["bulk@upstream", "exec@upstream"] + MODES
    modes = [m for m in order if m in found] + sorted(m for m in found if m not in order)
    R = {m: load(out, m) for m in modes}
    _, failing = filter_lists(False)
    tests = sorted(set().union(*[set(r) for r in R.values()]))
    lines = []
    w = lines.append
    w("# SERVPIPS V1: test262 ES5 subset (javert-test262 %s)\n" % TEST262_REV[:12])
    imgs = {m: sorted({r.get("image") for r in R[m].values()}) for m in modes}
    w("Images: " + "; ".join("%s=%s" % (m, ",".join(i for i in imgs[m] if i)) for m in modes) + "\n")
    w("## Totals\n")
    w("| mode | tests | pass | fail | skip | fail classes |")
    w("|---|---|---|---|---|---|")
    for m in modes:
        rs = R[m].values()
        c = collections.Counter(r["verdict"] for r in rs)
        cls = collections.Counter(re.sub(r"\(.*", "(..)", r["class"])
                                  for r in rs if r["verdict"] == "fail")
        w("| %s | %d | %d | %d | %d | %s |" % (m, len(R[m]), c["pass"], c["fail"], c["skip"],
                                         ", ".join("%s %d" % kv for kv in cls.most_common())))
    in_fail = lambda t: any(s in t for s in failing)
    w("\n(Run tests matching a `failing_tests` pattern of Test262_filtering.ml, which `--ci` "
      "filters out: %d.)\n" % sum(1 for t in tests if in_fail(t)))
    w("## Failure clusters (class and normalised detail)\n")
    w("| mode | count | class | detail (numbers replaced by N) | example |")
    w("|---|---|---|---|---|")
    for m in modes:
        cl, ex = collections.Counter(), {}
        for t, r in R[m].items():
            if r["verdict"] != "fail":
                continue
            d = re.sub(r"(?<![A-Za-z$])\d+(\.\d+)?", "N", (r.get("detail") or "").replace("\n", " "))
            k = (re.sub(r"\(.*", "(..)", r["class"]), d[:110].replace("|", "\\|"))
            cl[k] += 1
            ex.setdefault(k, t)
        for k, c in cl.most_common():
            w("| %s | %d | %s | %s | %s |" % (m, c, k[0], k[1], ex[k]))

    w("## Per folder (pass / run)\n")
    w("| folder | " + " | ".join(modes) + " |")
    w("|---|" + "---|" * len(modes))
    groups = collections.OrderedDict()
    for t in tests:
        groups.setdefault(group_of(t), []).append(t)
    for g, ts in groups.items():
        cells = []
        for m in modes:
            run = [t for t in ts if t in R[m] and R[m][t]["verdict"] != "skip"]
            ok = [t for t in run if R[m][t]["verdict"] == "pass"]
            cells.append("%d/%d" % (len(ok), len(run)))
        w("| %s | %s |" % (g, " | ".join(cells)))

    def v(m, t):
        r = R[m].get(t)
        return r["verdict"] if r else None

    def desc(m, t):
        r = R[m].get(t)
        if not r:
            return "-"
        s = r["class"]
        if r.get("detail"):
            s += ": " + r["detail"].replace("\n", " ").replace("|", "\\|")[:160]
        return s

    base = "exec"
    sections = [
        ("Upstream master vs this image, both without --servpips (bulk)", "bulk@upstream", "bulk", None),
        ("Upstream master vs this image, both without --servpips (exec)", "exec@upstream", "exec", None),
        ("CLI runner vs upstream bulk runner (exec vs bulk)", "bulk", "exec", None),
        ("Regressions: pass in exec (upstream), fail in exec --servpips", base, "exec-servpips", "pass"),
        ("Regressions: pass in exec (upstream), fail in wpst --servpips", base, "wpst-servpips", "pass"),
        ("Improvements: fail in exec (upstream), pass in exec --servpips", base, "exec-servpips", "fail"),
        ("Improvements: fail in exec (upstream), pass in wpst --servpips", base, "wpst-servpips", "fail"),
        ("Mode inconsistencies: exec --servpips vs wpst --servpips", "exec-servpips", "wpst-servpips", None),
    ]
    for title, m1, m2, only in sections:
        if m1 not in R or m2 not in R:
            continue
        diff = [t for t in tests if v(m1, t) in ("pass", "fail") and v(m2, t) in ("pass", "fail")
                and v(m1, t) != v(m2, t) and (only is None or v(m1, t) == only)]
        w("\n## %s: %d\n" % (title, len(diff)))
        if not diff:
            continue
        extra = [m for m in ("wpst",) if m in R and "wpst-servpips" in (m1, m2)]
        w("| test | %s |" % " | ".join([m1, m2] + extra))
        w("|---|---|---|" + "---|" * len(extra))
        for t in diff:
            w("| %s%s | %s |" % (t, " (failing_tests)" if in_fail(t) else "",
                                 " | ".join(desc(m, t) for m in [m1, m2] + extra)))
    # timing
    w("\n## Time per test (seconds)\n")
    w("| mode | total | median | p99 | max | slowest |")
    w("|---|---|---|---|---|---|")
    for m in modes:
        ss = sorted((r["secs"], t) for t, r in R[m].items() if "secs" in r)
        if not ss:
            continue
        xs = [s for s, _ in ss]
        w("| %s | %.0f | %.2f | %.1f | %.1f | %s |" % (
            m, sum(xs), xs[len(xs) // 2], xs[min(len(xs) - 1, int(len(xs) * 0.99))],
            xs[-1], ss[-1][1]))
    open(os.path.join(out, "report.md"), "w").write("\n".join(lines) + "\n")
    with open(os.path.join(out, "verdicts.tsv"), "w") as fh:
        fh.write("test\t" + "\t".join(modes) + "\n")
        for t in tests:
            fh.write(t + "\t" + "\t".join(
                "%s:%s" % (R[m][t]["verdict"], R[m][t]["class"]) if t in R[m] else "-"
                for m in modes) + "\n")
    log("wrote %s" % os.path.join(out, "report.md"))


def cmd_fetch(a):
    d = os.path.abspath(a.dir)
    if not os.path.isdir(os.path.join(d, ".git")):
        subprocess.run(["git", "clone", "-q", TEST262_REPO, d], check=True)
    subprocess.run(["git", "-C", d, "checkout", "-q", TEST262_REV], check=True)
    log("test262 at %s in %s" % (TEST262_REV, d))


def cmd_all(a):
    if not os.path.isdir(os.path.join(a.suite, "test")):
        cmd_fetch(argparse.Namespace(dir=a.suite))
    cmd_run(argparse.Namespace(**dict(vars(a), mode="exec", ref=None, label=None)))
    for m in ("bulk", "exec-servpips", "wpst-servpips"):
        cmd_run(argparse.Namespace(**dict(vars(a), mode=m, ref="exec", label=None,
                                          skip_ref_timeouts=True)))
    # diagnosis: the fork's wpst without SERVPIPS mode on the tests that pass
    # under exec and fail under wpst --servpips
    R1, R2 = load(a.out, "exec"), load(a.out, "wpst-servpips")
    diag = sorted(t for t in R2 if R2[t]["verdict"] == "fail"
                  and R1.get(t, {}).get("verdict") == "pass")
    tf = os.path.join(a.out, "wpst-diagnosis.tests")
    open(tf, "w").write("".join(t + "\n" for t in diag))
    cmd_run(argparse.Namespace(**dict(vars(a), mode="wpst", ref=None, label=None,
                                      tests_file=tf)))
    cmd_report(a)


def main():
    ap = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    sp = ap.add_subparsers(dest="cmd", required=True)
    p = sp.add_parser("fetch")
    p.add_argument("--dir", required=True)
    for name in ("run", "all"):
        p = sp.add_parser(name)
        p.add_argument("--suite", required=True, help="javert-test262 checkout")
        p.add_argument("--out", required=True)
        p.add_argument("--image", default=default_image())
        p.add_argument("--jobs", type=int, default=8)
        p.add_argument("--memory", default="6g", help="per container")
        p.add_argument("--ci", action="store_true", help="also filter failing_tests")
        p.add_argument("--fresh", action="store_true", help="ignore earlier results")
        p.add_argument("--timeout", type=int, default=0,
                       help="seconds per test (default 180; 900 for wpst)")
        p.add_argument("--ref", default=None,
                       help="earlier result (mode[@label]) whose times order the run "
                            "(slowest first)")
        p.add_argument("--skip-ref-timeouts", action="store_true",
                       help="skip (record as skip) the tests that timed out under --ref")
        p.add_argument("--only", action="append", help="path substring (repeatable)")
        p.add_argument("--limit", type=int, default=0)
        p.add_argument("--tests-file", default=None,
                       help="only the tests listed (paths relative to test/, one per line)")
        p.add_argument("--redo", action="store_true",
                       help="with --tests-file: run the listed tests again")
        if name == "run":
            p.add_argument("--mode", required=True, choices=MODES)
            p.add_argument("--label", default=None,
                           help="results go to O/<mode>@<label>.jsonl (e.g. another image)")
    p = sp.add_parser("report")
    p.add_argument("--out", required=True)
    a = ap.parse_args()
    {"fetch": cmd_fetch, "run": cmd_run, "report": cmd_report,
     "all": cmd_all}[a.cmd](a)


if __name__ == "__main__":
    main()
