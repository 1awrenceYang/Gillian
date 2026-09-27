#!/usr/bin/env bash
# Run the SERVPIPS regression examples Gillian-JS/Examples/Cosette/servpips_*.js
# and compare the JSONL events they produce (all lines after the "hello" line)
# with <name>.expected.jsonl.
#
# The comparison ignores the numbering of logical variables and abstract
# locations: in every string of every event, names #gen__N, #lvar_N, _lvar_N,
# #loc_N and _$l_N are renumbered in order of first appearance in the file
# (per prefix), so distinct names stay distinct and equal names stay equal.
#
# usage: servpips_examples.sh [-i IMAGE] [-g CMD] [-j JOBS] [-u] [-k] [-v] [PATTERN...]
#   -i IMAGE  engine image (docker; entrypoint gillian-js), default
#             servpips-gillian:<first 12 hex digits of the fork HEAD>
#   -g CMD    run gillian-js as "CMD DIR ARGS..." (DIR = working directory of
#             the example) instead of docker, e.g. a development build
#   -j JOBS   examples run in parallel (default 4)
#   -u        update: write <name>.expected.jsonl from the actual events
#             (review the diff before committing it)
#   -k        keep the work directory
#   -v        print the diff of failing examples
#   PATTERN   only run examples whose file name contains one of the patterns
#
# Conventions (servpips_<package>_<name>.js; packages core, mem, rt, s0):
#   - a file with <name>.expected.jsonl is a wpst test, run as
#       gillian-js wpst <file> --servpips --servpips-log events.jsonl \
#         -l disabled --result-dir .gillian [ARGS]
#     and must exit with code 0 unless a "servpips-example: rc=N" directive
#     says otherwise;
#   - a file named *_exec.js, or containing "servpips-example: exec", is a
#     concrete-execution test (gillian-js exec) that must exit with code 0;
#   - a file containing "servpips-example: wpst" is a wpst test even before
#     its .expected.jsonl exists (use -u to create it);
#   - directives (in // line comments; the value runs to the end of line):
#       servpips-example: rc=N       expected exit code of wpst
#       servpips-example: args=...   extra gillian-js arguments (rest of line)
#       servpips-example: skip       not a test
#   - every other servpips_*.js file is a helper module and is not run.
# All servpips_*.js files are copied to <work>/src, and each example runs in
# its own directory <work>/run/<name> as ../../src/<name>.js.
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
    *) sed -n '2,40p' "$0"; exit 2 ;;
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
cp "$EX_DIR"/servpips_*.expected.jsonl "$WORK/src/" 2>/dev/null
chmod -R a+rwX "$WORK"

directive() { # directive FILE KEY -> value ("yes" for a bare directive), or empty
  grep -o "servpips-example: *$2\(=.*\)\{0,1\}\$" "$1" | head -1 \
    | sed "s/^servpips-example: *$2//; s/^=//; s/^\$/yes/"
}

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
  local name=$1 src=$WORK/src/$1.js dir=$WORK/run/$1
  mkdir -p "$dir"; chmod 777 "$dir"
  local extra rc exp_rc mode
  extra=$(directive "$src" args)
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
  # shellcheck disable=SC2086
  run_gillian "$name" wpst "../../src/$name.js" --servpips --servpips-log events.jsonl \
    -l disabled --result-dir .gillian $extra > "$dir/stdout" 2>&1
  rc=$?
  exp_rc=$(directive "$src" rc); exp_rc=${exp_rc:-0}
  cp "$dir/stdout" "$WORK/res/$name.log"
  if [ ! -f "$dir/events.jsonl" ]; then
    echo "FAIL (no events.jsonl, rc=$rc)" > "$WORK/res/$name.status"; return
  fi
  tail -n +2 "$dir/events.jsonl" > "$dir/actual.jsonl"
  if [ "$UPDATE" = 1 ]; then
    cp "$dir/actual.jsonl" "$EX_DIR/$name.expected.jsonl"
  fi
  local verdict
  verdict=$(python3 - "$dir/actual.jsonl" "$WORK/src/$name.expected.jsonl" "$dir/diff.txt" <<'PY'
import json, re, sys, difflib
PAT = re.compile(r'(#gen__|#lvar_|_lvar_|#loc_|_\$l_)(\d+)')
def canon(path):
    try:
        lines = [l for l in open(path).read().splitlines() if l.strip()]
    except FileNotFoundError:
        return None
    maps = {}
    def ren(m):
        d = maps.setdefault(m.group(1), {})
        return m.group(1) + '@' + str(d.setdefault(m.group(2), len(d) + 1))
    def walk(x):
        if isinstance(x, str): return PAT.sub(ren, x)
        if isinstance(x, list): return [walk(y) for y in x]
        if isinstance(x, dict): return {k: walk(v) for k, v in x.items()}
        return x
    out = []
    for l in lines:
        try: out.append(json.dumps(walk(json.loads(l)), sort_keys=True))
        except ValueError: out.append('UNPARSEABLE ' + l)
    return out
a = canon(sys.argv[1]); e = canon(sys.argv[2])
if e is None:
    print('no expected file'); sys.exit(0)
if a == e:
    print('same'); sys.exit(0)
open(sys.argv[3], 'w').write('\n'.join(difflib.unified_diff(e, a, 'expected', 'actual', lineterm='')))
print('different')
PY
)
  if [ "$UPDATE" = 1 ]; then
    echo "UPDATED (rc=$rc, expected rc $exp_rc)" > "$WORK/res/$name.status"
  elif [ "$verdict" = same ] && [ "$rc" = "$exp_rc" ]; then
    echo PASS > "$WORK/res/$name.status"
  elif [ "$verdict" = same ]; then
    echo "FAIL (rc=$rc, expected $exp_rc)" > "$WORK/res/$name.status"
  else
    echo "FAIL (events $verdict, rc=$rc)" > "$WORK/res/$name.status"
  fi
}

TESTS=()
for f in "$WORK"/src/servpips_*.js; do
  name=$(basename "$f" .js)
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
         [ -f "$WORK/run/$t/diff.txt" ] && { sed 's/^/    /' "$WORK/run/$t/diff.txt"; echo; }
         tail -5 "$WORK/res/$t.log" | sed 's/^/    | /'
       fi ;;
  esac
done
echo "servpips examples: PASS=$PASS FAIL=$FAIL (work: $WORK)"
[ "$KEEP" = 1 ] || rm -rf "$WORK"
[ $FAIL = 0 ]
