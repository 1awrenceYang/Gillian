#!/usr/bin/env bash
# Run the SERVPIPS regression examples Gillian-JS/Examples/Cosette/servpips_*.js
# and compare the JSONL events they produce with <name>.expected.jsonl.
#
# usage: servpips_examples.sh [-i IMAGE] [-g CMD] [-j JOBS] [-u] [-k] [-v] [PATTERN...]
#   -i IMAGE  engine image (docker; entrypoint gillian-js), default
#             servpips-gillian:<first 12 hex digits of the fork HEAD>
#   -g CMD    run gillian-js as "CMD DIR ARGS..." (DIR = working directory of
#             the example, <work>/run/<name>; <work> must be visible to CMD)
#             instead of docker, e.g. a development build
#   -j JOBS   examples run in parallel (default 4)
#   -u        update: write <name>.expected.jsonl from the actual events
#             (the selected kinds, normalised stats; review before committing)
#   -k        keep the work directory
#   -v        print the diff and output tail of failing examples
#   PATTERN   only run examples whose file name contains one of the patterns
#
# Kinds of examples (servpips_<package>_<name>.js; packages core, mem, rt, s0;
# hand-written GIL probes servpips_<package>_<name>.gil are wpst tests run
# with -a):
#   - wpst test: the file has <name>.expected.jsonl, or contains the directive
#     "servpips-example: wpst". It is run as
#       gillian-js wpst ../../src/<name>.js --servpips --servpips-log events.jsonl \
#         -l disabled --result-dir .gillian --unroll 2000 --smt-timeout 5000 [ARGS]
#     (the two defaults are left out when ARGS gives them) and must exit with
#     code 0 unless "servpips-example: rc=N" says otherwise;
#   - exec test: a file named *_exec.js, or containing "servpips-example: exec",
#     is run with gillian-js exec and must exit with code 0;
#   - every other servpips_*.js file is a helper module (e.g. *_mod.js).
# Directives (either spelling; the value runs to the end of the comment/line):
#   /* SERVPIPS-ARGS: <extra gillian-js args> */   or  // servpips-example: args=...
#   /* SERVPIPS-EVENTS: <kinds> */                    event kinds compared:
#       comma list of hello, decl, lazy, call, note, prune, prune:<by>, end,
#       stats (default: every kind except hello and prune)
#   // servpips-example: rc=N | wpst | exec | skip
#
# Comparison: the selected events of the log and of the expected file, in
# order, after normalisation: logical variables and abstract locations
# (#gen__N, #lvar_N, #loc_N, ..., _$l_N, $l_N) are renumbered per prefix in
# order of first appearance; stats keep only their deterministic fields
# (leaves, ends, infeasible, vanished, max_branch, fatal, solver.{
# unknown_assumed_sat, entail_unknown, encode_failures}). When the log ends
# with a stats line, the integrity invariants of the design (section 5.1)
# are checked too (ends per status, leaves = ends + infeasible,
# truncated >= max_branch, prunes and vanished counts).
# All servpips_*.js files are copied to <work>/src; each example runs in its
# own directory <work>/run/<name>.
set -u
HERE=$(cd "$(dirname "$0")" && pwd)
EX_DIR=$HERE/../Examples/Cosette
IMG=""
CMD=""
JOBS=4
UPDATE=0
KEEP=0
VERBOSE=0
while getopts "i:g:j:ukvh" opt; do
  case $opt in
    i) IMG=$OPTARG ;;
    g) CMD=$OPTARG ;;
    j) JOBS=$OPTARG ;;
    u) UPDATE=1 ;;
    k) KEEP=1 ;;
    v) VERBOSE=1 ;;
    *) sed -n '2,48p' "$0"; exit 2 ;;
  esac
done
shift $((OPTIND - 1))
PATTERNS=("$@")
if [ -z "$CMD" ] && [ -z "$IMG" ]; then
  IMG="servpips-gillian:$(git -C "$HERE" rev-parse --short=12 HEAD)"
fi

WORK=$(mktemp -d "${TMPDIR:-/tmp}/servpips-examples.XXXXXX")
chmod 777 "$WORK"
mkdir -p "$WORK/src" "$WORK/run" "$WORK/res"
cp "$EX_DIR"/servpips_*.js "$WORK/src/"
cp "$EX_DIR"/servpips_*.gil "$WORK/src/" 2>/dev/null
cp "$EX_DIR"/servpips_*.expected.jsonl "$WORK/src/" 2>/dev/null
chmod -R a+rwX "$WORK"

# directive FILE KEY -> value ("yes" for a bare directive), or empty
directive() {
  grep -o "servpips-example: *$2\\(=.*\\)\\{0,1\\}\$" "$1" | head -1 \
    | sed "s/^servpips-example: *$2//; s/^=//; s/^\$/yes/"
}
# header FILE NAME -> value of /* NAME: value */, or empty
header() {
  sed -n "s/.*$2: *\\(.*[^ ]\\) *\\*[/)].*/\\1/p" "$1" | head -1
}

cat > "$WORK/events.py" <<'PY'
import json, re, sys, difflib
STATS_KEEP = ["leaves", "ends", "infeasible", "vanished", "max_branch", "fatal"]
SOLVER_KEEP = ["unknown_assumed_sat", "entail_unknown", "encode_failures"]
NAME = re.compile(r'(#[A-Za-z_]+|_\$l_|\$l_)(\d+)')

def load(path):
    out = []
    for line in open(path, encoding="utf-8"):
        if not line.endswith("\n"):
            break  # incomplete last line (killed run)
        if line.strip():
            try:
                out.append(json.loads(line))
            except ValueError:
                out.append({"ev": "UNPARSEABLE", "text": line.strip()})
    return out

def selected(ev, kinds):
    k = ev.get("ev")
    if kinds is None:
        return k not in ("hello", "prune")
    return k in kinds or (k == "prune" and ("prune:" + str(ev.get("by"))) in kinds)

def filt(evs, kinds):
    res = []
    for ev in evs:
        if not selected(ev, kinds):
            continue
        if ev.get("ev") == "stats":
            s = {k: ev[k] for k in ["ev"] + STATS_KEEP if k in ev}
            s["solver"] = {k: ev.get("solver", {}).get(k) for k in SOLVER_KEEP}
            ev = s
        res.append(ev)
    return res

def normalise(evs):
    maps = {}
    def ren(m):
        d = maps.setdefault(m.group(1), {})
        return m.group(1) + "@" + str(d.setdefault(m.group(2), len(d) + 1))
    def walk(x):
        if isinstance(x, str): return NAME.sub(ren, x)
        if isinstance(x, list): return [walk(y) for y in x]
        if isinstance(x, dict): return {k: walk(v) for k, v in x.items()}
        return x
    return [json.dumps(walk(ev), sort_keys=True) for ev in evs]

def integrity(evs):
    if not evs or evs[-1].get("ev") != "stats":
        return None
    st = evs[-1]
    ends = [e for e in evs if e.get("ev") == "end"]
    byst = {}
    for e in ends:
        byst[e["status"]] = byst.get(e["status"], 0) + 1
    for k, v in st.get("ends", {}).items():
        if byst.get(k, 0) != v:
            return "ends[%s] = %s but %s end events" % (k, v, byst.get(k, 0))
    if st["leaves"] != sum(st["ends"].values()) + st["infeasible"]:
        return "leaves != ends + infeasible"
    if st["ends"].get("truncated", 0) < st.get("max_branch", 0):
        return "truncated < max_branch"
    if "prunes" in st and st["prunes"] != sum(1 for e in evs if e.get("ev") == "prune"):
        return "prunes count"
    if st.get("vanished", 0) != sum(1 for e in ends if e.get("reason") == "vanished"):
        return "vanished count"
    return None

cmd, actual, expected, kinds_s, out = sys.argv[1:6]
kinds = None if kinds_s == "" else set(kinds_s.split(","))
evs = load(actual)
if len(evs) > 0 and evs[0].get("ev") == "hello":
    evs = evs[1:]
got = filt(evs, kinds)
if cmd == "update":
    with open(out, "w") as f:
        for ev in got:
            f.write(json.dumps(ev, ensure_ascii=False) + "\n")
    print("updated")
    sys.exit(0)
bad = integrity(evs)
if bad:
    print("integrity: " + bad)
    sys.exit(0)
try:
    exp = filt(load(expected), kinds)
except FileNotFoundError:
    print("no expected file")
    sys.exit(0)
g, e = normalise(got), normalise(exp)
if g == e:
    print("same")
else:
    open(out, "w").write("\n".join(difflib.unified_diff(e, g, "expected", "actual", lineterm="")) + "\n")
    print("different")
PY

run_gillian() { # run_gillian NAME ARGS...
  local name=$1; shift
  local dir=$WORK/run/$name
  if [ -n "$CMD" ]; then
    $CMD "$dir" "$@"
  else
    docker run --rm --name "sp-examples-$$-$name" --user "$(id -u):$(id -g)" \
      --network none --memory 8g -v "$WORK:$WORK" -w "$dir" "$IMG" "$@"
  fi
}

run_one() { # run_one NAME ; writes $WORK/res/NAME.{status,log}
  local name=$1 ext=js dir=$WORK/run/$1
  [ -f "$WORK/src/$1.gil" ] && ext=gil
  local src=$WORK/src/$1.$ext
  mkdir -p "$dir"; chmod 777 "$dir"
  local extra rc exp_rc mode kinds defaults
  extra="$(header "$src" SERVPIPS-ARGS) $(directive "$src" args)"
  # hand-written GIL probes (servpips_*.gil) run with -a
  if [ "$ext" = gil ]; then case " $extra " in *" -a "*) ;; *) extra="$extra -a" ;; esac; fi
  kinds=$(header "$src" SERVPIPS-EVENTS)
  mode=wpst
  case "$name" in *_exec) mode=exec ;; esac
  [ "$(directive "$src" exec)" = yes ] && mode=exec
  if [ "$mode" = exec ]; then
    # shellcheck disable=SC2086
    run_gillian "$name" exec "../../src/$name.js" -l disabled --result-dir .gillian $extra \
      > "$dir/stdout" 2>&1
    rc=$?
    if [ $rc = 0 ]; then echo PASS > "$WORK/res/$name.status"
    else echo "FAIL (exec rc=$rc)" > "$WORK/res/$name.status"; fi
    cp "$dir/stdout" "$WORK/res/$name.log"
    return
  fi
  defaults=""
  case " $extra " in *" --unroll "*) ;; *) defaults="$defaults --unroll 2000" ;; esac
  case " $extra " in *" --smt-timeout "*) ;; *) defaults="$defaults --smt-timeout 5000" ;; esac
  # shellcheck disable=SC2086
  run_gillian "$name" wpst "../../src/$name.$ext" --servpips --servpips-log events.jsonl \
    -l disabled --result-dir .gillian $defaults $extra > "$dir/stdout" 2>&1
  rc=$?
  exp_rc=$(directive "$src" rc); exp_rc=${exp_rc:-0}
  cp "$dir/stdout" "$WORK/res/$name.log"
  if [ ! -f "$dir/events.jsonl" ]; then
    echo "FAIL (no events.jsonl, rc=$rc)" > "$WORK/res/$name.status"; return
  fi
  local verdict
  if [ "$UPDATE" = 1 ]; then
    python3 "$WORK/events.py" update "$dir/events.jsonl" - "$kinds" "$EX_DIR/$name.expected.jsonl" > /dev/null
    echo "UPDATED (rc=$rc, expected rc $exp_rc)" > "$WORK/res/$name.status"
    return
  fi
  verdict=$(python3 "$WORK/events.py" compare "$dir/events.jsonl" "$WORK/src/$name.expected.jsonl" \
    "$kinds" "$dir/diff.txt")
  if [ "$verdict" = same ] && [ "$rc" = "$exp_rc" ]; then
    echo PASS > "$WORK/res/$name.status"
  elif [ "$verdict" = same ]; then
    echo "FAIL (rc=$rc, expected $exp_rc)" > "$WORK/res/$name.status"
  else
    echo "FAIL (events $verdict, rc=$rc)" > "$WORK/res/$name.status"
  fi
}

TESTS=()
for f in "$WORK"/src/servpips_*.js "$WORK"/src/servpips_*.gil; do
  [ -f "$f" ] || continue
  name=$(basename "$f"); name=${name%.js}; name=${name%.gil}
  if [ ${#PATTERNS[@]} -gt 0 ]; then
    keep=0
    for p in "${PATTERNS[@]}"; do case "$name" in *"$p"*) keep=1 ;; esac; done
    [ $keep = 1 ] || continue
  fi
  [ "$(directive "$f" skip)" = yes ] && continue
  if [ -f "$WORK/src/$name.expected.jsonl" ] || [ "$(directive "$f" wpst)" = yes ] \
     || [ "$(directive "$f" exec)" = yes ]; then
    TESTS+=("$name")
  else
    case "$name" in *_exec) TESTS+=("$name") ;; esac
  fi
done

running=0
for t in "${TESTS[@]}"; do
  run_one "$t" &
  running=$((running + 1))
  if [ $running -ge "$JOBS" ]; then wait -n; running=$((running - 1)); fi
done
wait

PASS=0; FAIL=0
for t in "${TESTS[@]}"; do
  st=$(cat "$WORK/res/$t.status" 2>/dev/null || echo "FAIL (no result)")
  echo "$st $t"
  case "$st" in
    PASS|UPDATED*) PASS=$((PASS + 1)) ;;
    *) FAIL=$((FAIL + 1))
       if [ "$VERBOSE" = 1 ]; then
         [ -f "$WORK/run/$t/diff.txt" ] && cut -c1-400 "$WORK/run/$t/diff.txt" | sed 's/^/    /'
         tail -5 "$WORK/res/$t.log" | sed 's/^/    | /'
       fi ;;
  esac
done
echo "servpips examples: PASS=$PASS FAIL=$FAIL (work: $WORK)"
[ "$KEEP" = 1 ] || rm -rf "$WORK"
[ $FAIL = 0 ]
