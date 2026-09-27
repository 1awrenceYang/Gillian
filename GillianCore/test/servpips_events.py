#!/usr/bin/env python3
"""Filter / normalise / compare / check SERVPIPS JSONL event logs (I1).

usage (python3 >= 3.9):
  servpips_events.py filter  <events.jsonl> <kinds>          -> filtered events on stdout
  servpips_events.py compare <events.jsonl> <expected.jsonl> <kinds>  -> rc 0 iff equal after normalisation
kinds: comma list among hello,decl,lazy,call,note,prune,prune:<by>,end,stats (default end,note,stats)
  servpips_events.py check   <events.jsonl>          -> integrity invariants of design 5.1 (rc 0 iff OK)
Normalisation: logical variables (#name) and abstract locations (_$l_N) are renamed
canonically by order of first appearance; stats keep only deterministic fields
(leaves, ends, infeasible, vanished, max_branch, fatal, solver unknown/encode counts).
Probe convention (Gillian-JS/Examples/Cosette/servpips_core_*): the first comment may
contain "SERVPIPS-ARGS: <extra wpst args>" and "SERVPIPS-EVENTS: <kinds>"; the
.expected.jsonl file holds the filtered events after the hello line.
"""
import json, re, sys

STATS_KEEP = ["leaves", "ends", "infeasible", "vanished", "max_branch", "fatal"]
SOLVER_KEEP = ["unknown_assumed_sat", "entail_unknown", "encode_failures"]

def load(path):
    out = []
    for i, line in enumerate(open(path, encoding="utf-8")):
        if not line.endswith("\n"):
            break  # incomplete last line (killed run)
        line = line.strip()
        if not line:
            continue
        out.append(json.loads(line))
    return out

def keep(ev, kinds):
    k = ev.get("ev")
    if k in kinds:
        return True
    if k == "prune":
        return ("prune:" + ev.get("by", "")) in kinds
    return False

def filt(evs, kinds):
    res = []
    for ev in evs:
        if not keep(ev, kinds):
            continue
        if ev["ev"] == "stats":
            ev = {k: ev[k] for k in ["ev"] + STATS_KEEP if k in ev} | \
                 {"solver": {k: ev.get("solver", {}).get(k) for k in SOLVER_KEEP}}
        res.append(ev)
    return res

VAR_RE = re.compile(r"(#[A-Za-z_][A-Za-z0-9_]*|_\$l_[0-9]+|\$l_[0-9]+)")

def normalise(evs):
    names = {}
    lines = []
    for ev in evs:
        s = json.dumps(ev, sort_keys=True, ensure_ascii=False)
        def ren(m):
            n = m.group(0)
            if n not in names:
                names[n] = ("#v%d" % len(names)) if n.startswith("#") else ("$L%d" % len(names))
            return names[n]
        lines.append(VAR_RE.sub(ren, s))
    return lines

def main():
    cmd = sys.argv[1]
    if cmd == "filter":
        kinds = set((sys.argv[3] if len(sys.argv) > 3 else "end,note,stats").split(","))
        for ev in filt(load(sys.argv[2]), kinds):
            print(json.dumps(ev, ensure_ascii=False))
    elif cmd == "compare":
        kinds = set((sys.argv[4] if len(sys.argv) > 4 else "end,note,stats").split(","))
        got = normalise(filt(load(sys.argv[2]), kinds))
        exp = normalise(filt(load(sys.argv[3]), kinds))
        if got == exp:
            sys.exit(0)
        import difflib
        for l in difflib.unified_diff(exp, got, "expected", "got", lineterm="", n=1):
            print(l[:400])
        sys.exit(1)
    elif cmd == "check":  # integrity invariants of section 5.1
        evs = load(sys.argv[2])
        assert evs and evs[-1]["ev"] == "stats", "last line is not stats"
        st = evs[-1]
        ends = [e for e in evs if e["ev"] == "end"]
        byst = {}
        for e in ends:
            byst[e["status"]] = byst.get(e["status"], 0) + 1
        for k, v in st["ends"].items():
            assert byst.get(k, 0) == v, ("ends mismatch", k, v, byst.get(k, 0))
        assert st["leaves"] == sum(st["ends"].values()) + st["infeasible"], "leaves != ends + infeasible"
        assert st["ends"]["truncated"] >= st["max_branch"], "truncated < max_branch"
        assert st["prunes"] == sum(1 for e in evs if e["ev"] == "prune"), "prunes count"
        assert st["vanished"] == sum(1 for e in ends if e["reason"] == "vanished"), "vanished count"
        print("integrity OK: leaves=%d ends=%s infeasible=%d prunes=%d fatal=%s" %
              (st["leaves"], st["ends"], st["infeasible"], st["prunes"], st["fatal"]))
    else:
        sys.exit("bad command")

main()
