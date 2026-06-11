#!/usr/bin/env bash
# Build the benchmarks with a given Idris compiler and run JMH with the GC
# profiler so allocation rates (gc.alloc.rate.norm, B/op) are captured.
#
# Usage: ./run.sh <label> [idris2-executable] [extra JMH args...]
#
#   <label>             names the result files (results/<label>.json|.txt)
#   [idris2-executable] compiler to build with (default: idris2 on PATH)
#   [extra JMH args]    appended to the JMH command line, e.g. -prof jfr
#
# Example before/after comparison:
#   ./run.sh baseline ~/bin/idris2-baseline/exec/idris2
#   ./run.sh mono     ~/bin/idris2-0.8.1-SNAPSHOT/exec/idris2
#   ./compare.sh results/baseline.json results/mono.json
set -euo pipefail

LABEL="${1:?usage: run.sh <label> [idris2-executable] [extra JMH args...]}"
IDRIS2="${2:-idris2}"
shift
[ "$#" -gt 0 ] && shift

# Resolve a relative compiler path against the caller's directory before cd-ing
case "$IDRIS2" in
  */*) IDRIS2="$(cd "$(dirname "$IDRIS2")" && pwd)/$(basename "$IDRIS2")" ;;
esac

cd "$(dirname "$0")"

MVN=../../mvnw
[ -x "$MVN" ] || MVN=mvn

# Clean Idris output too: TTCs from a different compiler must not be reused
rm -rf build
"$MVN" -q clean package -Didris2.executable="$IDRIS2"

mkdir -p results
java -jar target/benchmark.jar -f 1 -wi 5 -i 5 -w 2 -r 2 -prof gc \
  -rf json -rff "results/${LABEL}.json" "$@" | tee "results/${LABEL}.txt"
