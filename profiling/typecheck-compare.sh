#!/usr/bin/env bash
# Interleaved JVM-vs-Chez timing of the shared typecheck workload.
#
# Usage: typecheck-compare.sh [-r rounds] [-l label]
#   -r rounds  number of (chez, jvm) pairs, default 2
#   -l label   tag for the results directory
#
# Env:
#   CHEZ_REF_DIR  chez reference worktree (default: <repo>/../idris-jvm-chez-ref,
#                 create with ./profiling/setup-chez.sh)
#   JVM_IDRIS2    JVM compiler to measure (default: <repo>/build/exec/idris2)
#
# Protocol (see README.md for the reasoning):
# - both sides typecheck IDENTICAL pristine source copies of the current
#   commit, clean build/ before every run
# - runs are interleaved (chez, jvm, chez, jvm, ...) so ambient load hits
#   both sides equally; the 1-minute load average is recorded before each run
# - runs that start above LOAD_WARN (default 10) are flagged; discard flagged
#   outliers rather than averaging them in
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

ROUNDS=2
LABEL=""
while getopts "r:l:h" opt; do
  case "$opt" in
    r) ROUNDS="$OPTARG" ;;
    l) LABEL="$OPTARG" ;;
    h) sed -n '2,20p' "$0"; exit 0 ;;
    *) exit 2 ;;
  esac
done

CHEZ_REF_DIR="${CHEZ_REF_DIR:-$REPO_ROOT/../idris-jvm-chez-ref}"
JVM_IDRIS2="${JVM_IDRIS2:-$REPO_ROOT/build/exec/idris2}"
LOAD_WARN="${LOAD_WARN:-10}"

CHEZ_IDRIS2="$CHEZ_REF_DIR/build/exec/idris2"
[ -x "$JVM_IDRIS2" ] || { echo "error: JVM compiler not found at $JVM_IDRIS2" >&2; exit 1; }
[ -x "$CHEZ_IDRIS2" ] || { echo "error: chez reference not found at $CHEZ_IDRIS2 (run ./profiling/setup-chez.sh)" >&2; exit 1; }

REPO_SHA="$(git -C "$REPO_ROOT" rev-parse HEAD)"
CHEZ_SHA="$(git -C "$CHEZ_REF_DIR" rev-parse HEAD 2>/dev/null || echo unknown)"
if [ "$REPO_SHA" != "$CHEZ_SHA" ]; then
  echo "warning: chez reference is at ${CHEZ_SHA:0:9}, repo is at ${REPO_SHA:0:9}." >&2
  echo "         The chez BINARY being older is usually fine (it is the yardstick);" >&2
  echo "         re-run setup-chez.sh if you want it rebuilt from HEAD." >&2
fi

TS="$(date +%Y%m%d-%H%M%S)"
RUN_DIR="$SCRIPT_DIR/results/$TS-$(git -C "$REPO_ROOT" rev-parse --short HEAD)-tc-compare${LABEL:+-$LABEL}"
mkdir -p "$RUN_DIR"
WL="$SCRIPT_DIR/workloads"
"$SCRIPT_DIR/typecheck-workload.sh" "$WL/chez" >/dev/null
"$SCRIPT_DIR/typecheck-workload.sh" "$WL/jvm" >/dev/null

load1() { uptime | sed 's/.*load average[s]*: *//' | cut -d' ' -f1 | tr -d ,; }

echo "Interleaved typecheck comparison, $ROUNDS round(s). Results: $RUN_DIR"
{
  echo "# repo=$REPO_SHA chez-ref=$CHEZ_SHA rounds=$ROUNDS date=$TS"
  for i in $(seq 1 "$ROUNDS"); do
    for side in chez jvm; do
      L="$(load1)"
      FLAG=""
      awk -v l="$L" -v w="$LOAD_WARN" 'BEGIN{exit !(l>w)}' && FLAG="  # LOAD-POISONED? started at load $L"
      echo "load-1min: $L"
      if [ "$side" = chez ]; then
        OUT="$(env -u IDRIS2_INC_CGS -u IDRIS2_PATH IDRIS2_PREFIX="$CHEZ_REF_DIR/bootstrap-build" \
               "$SCRIPT_DIR/typecheck-time.sh" "chez-$i" "$CHEZ_IDRIS2" "$WL/chez")" || exit 1
      else
        OUT="$(env -u IDRIS2_INC_CGS -u IDRIS2_PATH -u IDRIS2_PREFIX \
               "$SCRIPT_DIR/typecheck-time.sh" "jvm-$i" "$JVM_IDRIS2" "$WL/jvm")" || exit 1
      fi
      echo "$OUT$FLAG"
    done
  done
} | tee "$RUN_DIR/compare.log"

echo
echo "Summary (discard any LOAD-POISONED rows before comparing):"
grep "typecheck-total" "$RUN_DIR/compare.log" | sed 's/^/  /'
