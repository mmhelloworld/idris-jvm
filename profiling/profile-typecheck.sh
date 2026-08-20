#!/usr/bin/env bash
# JFR-profile a clean `--typecheck idris2.ipkg` run of the JVM compiler and
# generate CPU + allocation hotspot reports.
#
# Usage: profile-typecheck.sh [-l label] [-b idris2-binary]
#   -l label   tag for the results directory
#   -b binary  compiler to profile (default: <repo>/build/exec/idris2)
#
# Wall clock of a profiled run is inflated by deep-stack sampling — use
# typecheck-compare.sh for timing, this script for attribution.
#
# Results in profiling/results/<ts>-<sha>-tc-profile[-label]/:
#   typecheck.jfr  raw recording (open in JMC for flame graphs)
#   cpu.md         phase split + inclusive/leaf hot-method tables
#   alloc.md       allocation by class and by (class, allocation site)
#
# Follow-up attribution (who calls a hot method):
#   jfr print --events jdk.ExecutionSample <run>/typecheck.jfr \
#     | python3 profiling/callers.py <method-substring>
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

LABEL=""
IDRIS2="$REPO_ROOT/build/exec/idris2"
while getopts "l:b:h" opt; do
  case "$opt" in
    l) LABEL="$OPTARG" ;;
    b) IDRIS2="$OPTARG" ;;
    h) sed -n '2,20p' "$0"; exit 0 ;;
    *) exit 2 ;;
  esac
done
[ -x "$IDRIS2" ] || { echo "error: compiler not found at $IDRIS2" >&2; exit 1; }

TS="$(date +%Y%m%d-%H%M%S)"
RUN_DIR="$SCRIPT_DIR/results/$TS-$(git -C "$REPO_ROOT" rev-parse --short HEAD)-tc-profile${LABEL:+-$LABEL}"
mkdir -p "$RUN_DIR"
WL="$SCRIPT_DIR/workloads/profile"
"$SCRIPT_DIR/typecheck-workload.sh" "$WL" >/dev/null

# Idris-generated code recurses deeply; the JFR default stack depth of 64
# truncates before the frame that owns the work. 1024 keeps attribution
# working at a tolerable recording size for the ~6-minute typecheck.
STACKDEPTH="${JFR_STACKDEPTH:-1024}"
export JAVA_OPTS="-XX:FlightRecorderOptions:stackdepth=$STACKDEPTH -XX:StartFlightRecording:settings=profile,filename=$RUN_DIR/typecheck.jfr,dumponexit=true,maxsize=2g ${JAVA_OPTS:-}"

echo "Profiling clean typecheck (binary: $IDRIS2)"
echo "Results: $RUN_DIR"
cd "$WL"
rm -rf build
SECONDS=0
env -u IDRIS2_INC_CGS -u IDRIS2_PATH -u IDRIS2_PREFIX "$IDRIS2" --typecheck idris2.ipkg > "$RUN_DIR/typecheck.log" 2>&1
STATUS=$?
WALL=$SECONDS
[ $STATUS -ne 0 ] && { echo "typecheck FAILED (exit $STATUS):" >&2; tail -15 "$RUN_DIR/typecheck.log" >&2; exit $STATUS; }
echo "Typecheck finished in ${WALL}s (profiled runs are inflated; do not use for timing)"

echo "Generating cpu.md"
jfr print --events jdk.ExecutionSample "$RUN_DIR/typecheck.jfr" 2>/dev/null \
  | python3 "$SCRIPT_DIR/jfr_report.py" --wall "$WALL" --mode typecheck --label "${LABEL:-tc}" \
  > "$RUN_DIR/cpu.md"
echo "Generating alloc.md"
jfr print --events jdk.ObjectAllocationSample "$RUN_DIR/typecheck.jfr" 2>/dev/null \
  | python3 "$SCRIPT_DIR/alloc_report.py" \
  > "$RUN_DIR/alloc.md"

echo
echo "Reports:"
echo "  $RUN_DIR/cpu.md"
echo "  $RUN_DIR/alloc.md"
