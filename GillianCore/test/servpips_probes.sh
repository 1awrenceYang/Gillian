#!/bin/bash
# Run SERVPIPS probe programs (default: Gillian-JS/Examples/Cosette/servpips_core_*.{js,gil})
# on an engine image and compare their JSONL events with the .expected.jsonl files.
# usage: GillianCore/test/servpips_probes.sh IMAGE [--update] [glob ...]
# Per-file headers: "SERVPIPS-ARGS: <extra wpst args>" (".gil" files also get -a) and
# "SERVPIPS-EVENTS: <comma list of kinds>" (default end,note,stats; see servpips_events.py).
set -u
IMG=$1; shift
ROOT=$(cd "$(dirname "$0")/../.." && pwd)
EX=$ROOT/Gillian-JS/Examples/Cosette
EV=$ROOT/GillianCore/test/servpips_events.py
UPDATE=0; PATS=()
for a in "$@"; do if [ "$a" = --update ]; then UPDATE=1; else PATS+=("$a"); fi; done
[ ${#PATS[@]} -eq 0 ] && PATS=("servpips_core_*.js" "servpips_core_*.gil")
W=${SERVPIPS_PROBES_WORK:-$(mktemp -d)}; mkdir -p $W; cp $EX/servpips_* $W/; chmod -R a+rwX $W
PASS=0; FAIL=0
for f in $(cd $W && ls ${PATS[@]} 2>/dev/null | sort -u); do
  b=${f%.js}; b=${b%.gil}
  [ -e $EX/$b.expected.jsonl ] || [ $UPDATE = 1 ] || { echo "SKIP $f (no expected file)"; continue; }
  args=$(sed -n 's/.*SERVPIPS-ARGS: *\(.*[^ ]\) *\*[/)].*/\1/p' $W/$f | head -1)
  case $f in *.gil) case " $args " in *" -a "*) ;; *) args="$args -a";; esac;; esac
  kinds=$(sed -n 's/.*SERVPIPS-EVENTS: *\([^ ]*\) *\*[/)].*/\1/p' $W/$f | head -1); kinds=${kinds:-end,note,stats}
  docker run --rm --user $(id -u):$(id -g) --network none --memory 8g -v $W:$W -w $W $IMG \
    wpst $f --servpips --servpips-log $W/$b.ev.jsonl -l disabled --result-dir .gillian_$b $args > $W/$b.out 2>&1
  if ! python3 $EV check $W/$b.ev.jsonl > $W/$b.check 2>&1; then FAIL=$((FAIL+1)); echo "FAIL $f (integrity)"; cat $W/$b.check; continue; fi
  if [ $UPDATE = 1 ]; then python3 $EV filter $W/$b.ev.jsonl $kinds > $EX/$b.expected.jsonl; echo "UPDATED $b.expected.jsonl"; continue; fi
  if python3 $EV compare $W/$b.ev.jsonl $EX/$b.expected.jsonl $kinds > $W/$b.diff; then PASS=$((PASS+1)); echo "PASS $f"
  else FAIL=$((FAIL+1)); echo "FAIL $f"; head -20 $W/$b.diff; fi
done
echo "PASS=$PASS FAIL=$FAIL (work dir $W)"
[ $FAIL = 0 ]
