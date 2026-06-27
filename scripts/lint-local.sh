#!/usr/bin/env bash
#
# Run GitHub super-linter locally, mirroring .github/workflows/ci-super-linter.yml.
#
# Usage:
#   scripts/lint-local.sh
#
# Requires Docker to be running.
#
# NOTE: This mirrors CI, which only lints files changed vs main. Doing that
# locally needs two tricks:
#
#   1. RUN_LOCAL makes super-linter force VALIDATE_ALL_CODEBASE=true, so it lints
#      every file it can see rather than a git diff.
#   2. super-linter `find`-walks the WHOLE mounted directory to build its file
#      list before any include filter applies. Mounting the repo root means
#      walking ~22k files (.git, build/, target/, ...) over macOS's slow osxfs
#      bind mount -- minutes of apparent hang with no output.
#
# So instead of mounting the repo, we stage just the changed files (plus the
# .github/linters rule configs) into a throwaway directory and mount THAT. The
# walk is then ~85 files and finishes in seconds. FILTER_REGEX_INCLUDE further
# restricts validation to the changed files so the copied rule configs aren't
# linted themselves. This is read-only; it won't touch your working tree.

set -euo pipefail

# Repo root (script lives in scripts/).
REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

# super-linter runs as root inside the container, but the bind-mounted repo is
# owned by the host user. Modern git refuses such repos ("dubious ownership"),
# which makes the changed-files git diff fail with "not a git repository".
# safe.directory is only honored from a real global/system config file (not -c
# or env), so we provide one via a dedicated, writable HOME directory. We mount
# a directory (not the bare file) so git/super-linter can still write to it
# without the "Resource busy" you get when rewriting a bind-mounted file inode.
LINT_HOME="$(mktemp -d)"
# Staging dir we actually mount: only the changed files + linter rule configs.
STAGE="$(mktemp -d)"
trap 'rm -rf "${LINT_HOME}" "${STAGE}"' EXIT
printf '[safe]\n\tdirectory = /tmp/lint\n' > "${LINT_HOME}/.gitconfig"
chmod -R 777 "${LINT_HOME}"

# Mirror CI scope: only lint files changed vs DEFAULT_BRANCH plus any uncommitted
# changes. Without this, RUN_LOCAL lints the entire tree and looks hung.
# (read loop instead of mapfile: macOS ships bash 3.2, which lacks mapfile)
DEFAULT_BRANCH="${DEFAULT_BRANCH:-main}"
CHANGED=()
while IFS= read -r _f; do
  [ -n "${_f}" ] && CHANGED+=("${_f}")
done < <(
  {
    git -C "${REPO_ROOT}" diff --name-only --diff-filter=d "${DEFAULT_BRANCH}...HEAD"
    git -C "${REPO_ROOT}" diff --name-only --diff-filter=d HEAD
  } | sort -u
)
if [ "${#CHANGED[@]}" -eq 0 ]; then
  echo "No files changed vs ${DEFAULT_BRANCH}; nothing to lint."
  exit 0
fi
echo "Linting ${#CHANGED[@]} file(s) changed vs ${DEFAULT_BRANCH}:"
printf '  %s\n' "${CHANGED[@]}"

# Stage the changed files (preserving relative paths) into STAGE, then mount
# STAGE instead of the whole repo. --files-from reads the path list; rsync
# recreates intermediate directories.
printf '%s\n' "${CHANGED[@]}" | rsync -a --files-from=- "${REPO_ROOT}/" "${STAGE}/"

# Copy the linter rule configs separately so results match CI's custom rules.
# (rsync --files-from won't recurse a listed directory, so copy it directly.)
if [ -d "${REPO_ROOT}/.github/linters" ]; then
  mkdir -p "${STAGE}/.github"
  cp -R "${REPO_ROOT}/.github/linters" "${STAGE}/.github/linters"
fi
chmod -R u+rwX "${STAGE}"

# super-linter enumerates files via git (`git ls-tree HEAD`), not `find`, so the
# staging dir must be a real git repo with the files committed -- otherwise it
# finds nothing and "passes" vacuously. Commit everything; FILTER_REGEX_INCLUDE
# then narrows validation back to just the changed files.
git -C "${STAGE}" init -q
git -C "${STAGE}" add -A
git -C "${STAGE}" -c user.email=lint@local -c user.name=lint-local \
  commit -qm 'lint snapshot' >/dev/null

# Build an anchored alternation regex of the changed paths, escaping regex
# metacharacters, for super-linter's FILTER_REGEX_INCLUDE (matched per file path).
INCLUDE_REGEX="$(printf '%s\n' "${CHANGED[@]}" \
  | sed 's/[][(){}.^$*+?|\\]/\\&/g' \
  | paste -sd '|' -)"
INCLUDE_REGEX="(${INCLUDE_REGEX})\$"

docker run --rm \
  -e LOG_LEVEL=VERBOSE \
  -e FILTER_REGEX_INCLUDE="${INCLUDE_REGEX}" \
  -w /tmp/lint \
  -e HOME=/lint-home \
  -v "${LINT_HOME}":/lint-home \
  -e RUN_LOCAL=true \
  -e VALIDATE_ALL_CODEBASE="${VALIDATE_ALL_CODEBASE:-false}" \
  -e VALIDATE_CPP=false \
  -e VALIDATE_JSCPD=false \
  -e VALIDATE_JAVASCRIPT_STANDARD=false \
  -e VALIDATE_GOOGLE_JAVA_FORMAT=false \
  -e VALIDATE_PYTHON=false \
  -e VALIDATE_PYTHON_MYPY=false \
  -e VALIDATE_PYTHON_PYLINT=false \
  -e VALIDATE_PYTHON_BLACK=false \
  -e VALIDATE_MARKDOWN=false \
  -e VALIDATE_BASH=false \
  -e VALIDATE_SHELL_SHFMT=false \
  -e DEFAULT_BRANCH="${DEFAULT_BRANCH}" \
  -e IGNORE_GENERATED_FILES=true \
  -e NATURAL_LANGUAGE_CONFIG_FILE=.textlintrc.yml \
  -e CSS_FILE_NAME=.stylelintrc.json \
  -e FILTER_REGEX_EXCLUDE='.*Test.java' \
  -v "${STAGE}":/tmp/lint \
  github/super-linter:v4
