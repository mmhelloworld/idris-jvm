#!/usr/bin/env bash
# Create a pristine copy of the compiler sources as a typecheck workload.
#
# Usage: typecheck-workload.sh <dest-dir>
#
# The workload is `idris2 --typecheck idris2.ipkg` on a clean checkout of
# src/ at the CURRENT commit — pure elaboration + TTC serialization, no
# codegen, so it is directly comparable across backends. The generated
# src/IdrisPaths.idr is copied from the working tree (git archive skips it);
# the same file must go to every workload copy so the sources are identical.
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
DEST="${1:?usage: typecheck-workload.sh <dest-dir>}"

if [ ! -f "$REPO_ROOT/src/IdrisPaths.idr" ]; then
  echo "error: $REPO_ROOT/src/IdrisPaths.idr missing (generated file); run: make -C '$REPO_ROOT' src/IdrisPaths.idr" >&2
  exit 1
fi

rm -rf "$DEST"
mkdir -p "$DEST"
git -C "$REPO_ROOT" archive HEAD src idris2.ipkg | tar -x -C "$DEST"
cp "$REPO_ROOT/src/IdrisPaths.idr" "$DEST/src/"
echo "workload ready: $DEST ($(git -C "$REPO_ROOT" rev-parse --short HEAD))"
