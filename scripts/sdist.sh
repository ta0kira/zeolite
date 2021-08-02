#!/bin/bash

set -u -e

cd "$(dirname "$0")/.."

sdist_file=$(cabal sdist | sed 1d)
[[ $? -eq 0 ]] || exit $?

nv_files=($(tar -tzf "$sdist_file" | cut -d/ -f2- | grep -v '/$' | fgrep -vxf <(git ls-files)))

if [[ -n "${nv_files-}" ]]; then
  echo "*** $(basename "$sdist_file") contains untracked files ***" 1>&2
  printf '%s\n' "${nv_files[@]}"
  rm -f "$sdist_file"
else
  echo "$sdist_file"
fi
