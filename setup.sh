#!/usr/bin/env bash

# Copyright 2020 Kevin P. Barry
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Author: Kevin P. Barry [ta0kira@gmail.com]

set -u -e

here=$PWD
cd "$(dirname "$0")"

root=$PWD
compiler_hs="$root/compiler/Cli/compiler"
compiler_bin="$root/zeolite"
modules=(
  "$root"
  "$root/lib/util"
  "$root/tests/visibility/internal"
  "$root/tests/visibility"
  "$root/tests/visibility2/internal"
  "$root/tests/visibility2"
  "$root/tests"
)

build_compiler() {
  local command=(ghc -O5 -i"$root/compiler" "$compiler_hs" -o "$compiler_bin")
  echo "${command[@]}" 1>&2
  "${command[@]}"
}

init_modules() {
  local command=("$compiler_bin" -f -r "${modules[@]}")
  echo "${command[@]}" 1>&2
  "${command[@]}"
}

run() {
  build_compiler
  init_modules
}

run "$@"
