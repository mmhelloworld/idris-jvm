#!/usr/bin/env bash
# One-time setup of a Chez Scheme reference compiler for cross-backend
# comparisons (see README.md, "Comparing against the Chez backend").
#
# Creates a git worktree of the CURRENT commit, bootstraps it with the chez
# codegen, and installs the libraries the compiler depends on into the
# worktree's bootstrap prefix. The result is a self-hosted chez-backend
# idris2 built from the same sources as your JVM-backend build, so
# `typecheck-compare.sh` measures backend differences, not source drift.
#
# Usage: setup-chez.sh [dest-dir]
#   dest-dir defaults to <repo>/../idris-jvm-chez-ref
#
# Requires: chezscheme (`brew install chezscheme` — the `chez` executable).
#
# Notes on why this script exists (rather than plain `make bootstrap`):
# - This fork's libraries contain jvm-only `%foreign` specifiers. Those are
#   validated at CODE GENERATION time only, so the chez compiler can
#   TYPECHECK all of them — but `make bootstrap` builds libs with
#   IDRIS2_INC_CGS=chez (incremental codegen), which fails on them. The
#   bootstrap is therefore allowed to fail after producing the compiler
#   binary, and the libraries are then typecheck-built and installed with
#   IDRIS2_INC_CGS unset.
set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
DEST="${1:-$REPO_ROOT/../idris-jvm-chez-ref}"
SCHEME_EXE="${SCHEME:-chez}"

if ! command -v "$SCHEME_EXE" >/dev/null; then
  echo "error: chez scheme not found (looked for '$SCHEME_EXE'; brew install chezscheme, or set SCHEME)" >&2
  exit 1
fi

SHA="$(git -C "$REPO_ROOT" rev-parse HEAD)"
if [ -d "$DEST" ]; then
  HAVE="$(git -C "$DEST" rev-parse HEAD 2>/dev/null || echo none)"
  if [ "$HAVE" = "$SHA" ] && "$DEST/build/exec/idris2" --version >/dev/null 2>&1 \
     && ls "$DEST"/bootstrap-build/idris2-*/prelude-* >/dev/null 2>&1; then
    echo "Chez reference at $DEST is already at $SHA with a working compiler."
    echo "To rebuild from scratch: rm -rf '$DEST' && git -C '$REPO_ROOT' worktree prune, then re-run."
    exit 0
  fi
  echo "Updating existing worktree at $DEST ($HAVE -> $SHA)"
  git -C "$DEST" checkout --detach "$SHA" || exit 1
else
  echo "Creating worktree at $DEST @ $SHA"
  git -C "$REPO_ROOT" worktree add --detach "$DEST" "$SHA" || exit 1
fi

cd "$DEST"

# The chez-compiled binary dlopens libidris2_support at startup; this fork's
# top-level `make support` is a no-op for the C library, so build it directly
# BEFORE bootstrapping (the bootstrap runs the freshly built compiler).
echo "== Building C support library"
make -C support/c > support-c.log 2>&1 || { echo "error: support/c build failed — see $DEST/support-c.log" >&2; exit 1; }
SUPPORT_LIB="$(find support/c -name 'libidris2_support.dylib' -o -name 'libidris2_support.so' | head -1)"
[ -n "$SUPPORT_LIB" ] || { echo "error: no shared support library under support/c" >&2; exit 1; }

copy_support() {
  for app in build/exec/*_app bootstrap-build/idris2-*/idris2_app; do
    [ -d "$app" ] && [ ! -e "$app/$(basename "$SUPPORT_LIB")" ] && cp "$SUPPORT_LIB" "$app/"
  done
  return 0
}

echo "== Bootstrapping with chez (this takes a while; library-build failures at"
echo "   the end are EXPECTED and handled below)"
make bootstrap SCHEME="$SCHEME_EXE" > bootstrap.log 2>&1 || true
copy_support

if [ ! -x build/exec/idris2 ]; then
  echo "error: bootstrap did not produce build/exec/idris2 — see $DEST/bootstrap.log" >&2
  tail -20 bootstrap.log >&2
  exit 1
fi

# If the bootstrap's own library phase died before the support library was in
# place, a second pass picks up where it left off, now able to run the binary.
if ! ./build/exec/idris2 --version >/dev/null 2>&1; then
  echo "== Support library was missing during bootstrap; retrying bootstrap"
  make bootstrap SCHEME="$SCHEME_EXE" >> bootstrap.log 2>&1 || true
  copy_support
fi

echo "== Verifying the chez compiler runs"
./build/exec/idris2 --version || { echo "error: chez idris2 does not run — see $DEST/bootstrap.log" >&2; exit 1; }

echo "== Typecheck-building and installing libraries (IDRIS2_INC_CGS unset:"
echo "   jvm-only %foreign specifiers pass typechecking, fail only codegen)"
unset IDRIS2_INC_CGS IDRIS2_PATH
export IDRIS2_PREFIX="$DEST/bootstrap-build"
for lib in prelude base linear network contrib test; do
  echo "   $lib"
  ( cd "libs/$lib" && "$DEST/build/exec/idris2" --install "$lib.ipkg" ) > "install-$lib.log" 2>&1 || {
    echo "error: installing $lib failed — see $DEST/install-$lib.log" >&2
    tail -10 "install-$lib.log" >&2
    exit 1
  }
done

echo "== Done. Chez reference compiler:"
echo "   binary : $DEST/build/exec/idris2"
echo "   prefix : $DEST/bootstrap-build   (pass as IDRIS2_PREFIX when running it)"
echo "   commit : $SHA"
echo
echo "Next: ./profiling/typecheck-compare.sh   (it finds this directory by default)"
