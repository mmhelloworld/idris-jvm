#!/usr/bin/env bash
# Profile a compiler build (make idris2-exec) with Java Flight Recorder.
#
# The boot compiler is what gets profiled — it is the JVM process doing the
# compiling. To measure the compile-time cost of changes on this branch, build
# the compiler with those changes first, install it as the boot compiler, then
# run this script.
#
# Usage: profile-build.sh [-c] [-t] [-l label]
#   -c        clean build (rm -rf build/ttc build/exec) — measures the full
#             build; default is a warm build, which re-runs only the
#             whole-program JVM codegen (where monomorphization runs) and is
#             the fast regression check
#   -t        timing only: no JFR, no report. Deep-stack sampling inflates
#             wall clock (~80% observed), so use -t runs when comparing wall
#             times and profiled runs when attributing them
#   -l label  short label recorded with the results (e.g. "before-fix")
#
# Env:
#   IDRIS2_BOOT  boot compiler launcher (default: ~/bin/idris2-0.8.1/exec/idris2,
#                the released bootstrap). To measure branch changes, point this
#                at a compiler built from the branch instead — the boot is the
#                process being profiled.
#
# Results land in profiling/results/<timestamp>-<sha>[-label]/:
#   build.jfr   flight recording
#   build.log   make output
#   report.md   phase split + hot methods (human-readable)
#   report.json machine-readable, used by compare.py
# and a row is appended to profiling/history.csv for trend tracking.
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

MODE=warm
LABEL=""
PROFILE=1
while getopts "ctl:h" opt; do
  case "$opt" in
    c) MODE=clean ;;
    t) PROFILE=0 ;;
    l) LABEL="$OPTARG" ;;
    h) sed -n '2,25p' "$0"; exit 0 ;;
    *) exit 2 ;;
  esac
done

BOOT="${IDRIS2_BOOT:-$HOME/bin/idris2-0.8.1/exec/idris2}"
if [ ! -x "$BOOT" ]; then
  echo "error: boot compiler not found at $BOOT (set IDRIS2_BOOT)" >&2
  exit 1
fi

TS="$(date +%Y%m%d-%H%M%S)"
SHA="$(git -C "$REPO_ROOT" rev-parse --short HEAD)"
BRANCH="$(git -C "$REPO_ROOT" rev-parse --abbrev-ref HEAD)"
RUN_DIR="$SCRIPT_DIR/results/$TS-$SHA${LABEL:+-$LABEL}"
mkdir -p "$RUN_DIR"

if [ "$MODE" = clean ]; then
  echo "Clean build: removing build/ttc and build/exec"
  rm -rf "$REPO_ROOT/build/ttc" "$REPO_ROOT/build/exec"
fi

STACKDEPTH="${JFR_STACKDEPTH:-2048}"
if [ "$PROFILE" = 1 ]; then
  # The launcher script appends its own -Xss/-Xms/-Xmx after JAVA_OPTS, so heap
  # settings here would be overridden; only the JFR flags are injected.
  # Idris-generated code recurses deeply; the JFR default stack depth of 64
  # truncates before reaching the compiler frame that owns the work, which
  # breaks phase attribution (everything lands in runtime/other). 2048 keeps
  # attribution working; override with JFR_STACKDEPTH if recordings get huge.
  export JAVA_OPTS="-XX:FlightRecorderOptions:stackdepth=$STACKDEPTH -XX:StartFlightRecording:settings=profile,filename=$RUN_DIR/build.jfr,dumponexit=true,maxsize=2g ${JAVA_OPTS:-}"
else
  STACKDEPTH=""
fi

[ "$PROFILE" = 1 ] && WHAT="Profiling" || WHAT="Timing"
echo "$WHAT $MODE build of idris2 (boot: $BOOT)"
echo "Results: $RUN_DIR"
SECONDS=0
make -C "$REPO_ROOT" idris2-exec IDRIS2_BOOT="$BOOT" 2>&1 | tee "$RUN_DIR/build.log"
BUILD_STATUS=${PIPESTATUS[0]}
WALL=$SECONDS
echo "Build finished in ${WALL}s (exit $BUILD_STATUS)"

BACKEND_PCT=""
if [ "$PROFILE" = 1 ]; then
  if [ ! -s "$RUN_DIR/build.jfr" ]; then
    echo "error: no JFR recording produced" >&2
    exit 1
  fi

  echo "Generating report..."
  # jfr print buffers its whole output in one StringBuilder and dies on the
  # 2GB array limit for multi-GB deep-stack recordings, regardless of heap.
  # Disassemble into per-chunk files and print them one at a time instead.
  PARTS="$(mktemp -d)"
  jfr disassemble --output "$PARTS" "$RUN_DIR/build.jfr" > /dev/null
  for part in "$PARTS"/*.jfr; do
    JAVA_TOOL_OPTIONS="-Xmx8g" \
    jfr print --stack-depth "$STACKDEPTH" --events jdk.ExecutionSample "$part" 2>/dev/null
  done \
    | python3 "$SCRIPT_DIR/jfr_report.py" \
        --json "$RUN_DIR/report.json" --wall "$WALL" --sha "$SHA" \
        --branch "$BRANCH" --mode "$MODE" --label "$LABEL" \
    > "$RUN_DIR/report.md"
  rm -rf "$PARTS"

  {
    echo "## jfr view hot-methods (exclusive, JDK view)"
    echo '```'
    jfr view hot-methods "$RUN_DIR/build.jfr" 2>&1 || true
    echo '```'
    echo "## jfr view gc"
    echo '```'
    jfr view gc "$RUN_DIR/build.jfr" 2>&1 | tail -20 || true
    echo '```'
  } >> "$RUN_DIR/report.md"

  BACKEND_PCT="$(python3 -c "
import json
r = json.load(open('$RUN_DIR/report.json'))
print(r['buckets'].get('jvm-backend', {}).get('pct', 0))
")"
fi

HISTORY="$SCRIPT_DIR/history.csv"
[ -f "$HISTORY" ] || echo "timestamp,sha,branch,mode,label,stackdepth,build_status,wall_seconds,jvm_backend_pct" > "$HISTORY"
echo "$TS,$SHA,$BRANCH,$MODE,$LABEL,$STACKDEPTH,$BUILD_STATUS,$WALL,$BACKEND_PCT" >> "$HISTORY"

echo
if [ "$PROFILE" = 1 ]; then
  echo "Wall: ${WALL}s | jvm-backend share: ${BACKEND_PCT}% | report: $RUN_DIR/report.md"
else
  echo "Wall: ${WALL}s (timing-only run, no profile)"
fi
exit "$BUILD_STATUS"
