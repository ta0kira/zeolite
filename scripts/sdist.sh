#!/bin/bash

set -u -e

cd "$(dirname "$0")/.."

required='/(README\.md|\.zeolite-module|.+\.(0r[ptx]|h|hpp|cc|cpp))$'
ignore='^editors/|^scripts/'
sdist_file=$(cabal sdist | sed 1d)
[[ $? -eq 0 ]] || exit $?

tracked_files=$(git ls-files)
sdist_files=$(tar -tzf "$sdist_file" | cut -d/ -f2- | grep -v '/$')

nv_files=($(echo "$sdist_files" | fgrep -vxf <(echo "$tracked_files") || true))
if [[ -n "${nv_files-}" ]]; then
  echo "*** $(basename "$sdist_file") contains untracked files ***" 1>&2
  printf '%s\n' "${nv_files[@]}"
  rm -f "$sdist_file"
  exit 1
fi

missing_files=($(echo "$tracked_files" | egrep "$required" | egrep -v "$ignore" | fgrep -vxf <(echo "$sdist_files") || true))
if [[ -n "${missing_files-}" ]]; then
  echo "*** $(basename "$sdist_file") is missing required files ***" 1>&2
  printf '%s\n' "${missing_files[@]}"
  rm -f "$sdist_file"
  exit 1
fi

echo "$sdist_file"
