#!/usr/bin/env bash
# Time one clean `idris2 --typecheck idris2.ipkg` run on a workload copy.
#
# Usage: typecheck-time.sh <label> <idris2-binary> <workload-dir>
#
# Removes the workload's build/ first so every run is a full clean
# typecheck. Prints one line: "<label> typecheck-total: <N>s". The compiler
# output lands in <workload-dir>/last-run.log (shown on failure).
set -uo pipefail

LABEL="${1:?usage: typecheck-time.sh <label> <idris2> <workdir>}"
IDRIS2="${2:?usage: typecheck-time.sh <label> <idris2> <workdir>}"
DIR="${3:?usage: typecheck-time.sh <label> <idris2> <workdir>}"

cd "$DIR"
rm -rf build
start=$(date +%s)
"$IDRIS2" --typecheck idris2.ipkg > "$DIR/last-run.log" 2>&1
status=$?
end=$(date +%s)
if [ $status -ne 0 ]; then
  echo "$LABEL FAILED (exit $status):" >&2
  tail -15 "$DIR/last-run.log" >&2
  exit $status
fi
echo "$LABEL typecheck-total: $((end - start))s"
