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

build_compiler() {
  local command=(ghc -i"$root/compiler" "$compiler_hs" -o "$compiler_bin")
  echo "${command[@]}" 1>&2
  "${command[@]}"
}

init_base() {
  local command=(
    "$compiler_bin"
    -c .
    -e ../base
    -e ../capture-thread/include
    -e ../base/builtin.cpp
    -e ../base/builtin.hpp
    -e ../base/category-header.hpp
    -e ../base/category-source.cpp
    -e ../base/category-source.hpp
    -e ../base/cycle-check.hpp
    -e ../base/function.hpp
    -e ../base/logging.cpp
    -e ../base/logging.hpp
    -e ../base/types.cpp
    -e ../base/types.hpp
    -e ../capture-thread/include/thread-capture.h
    -e ../capture-thread/include/thread-crosser.h
    -e ../capture-thread/src/thread-crosser.cc)
  echo "${command[@]}" 1>&2
  "${command[@]}"
}

init_util() {
  local command=(
    "$compiler_bin"
    -c util
    -e ../Category_SimpleOutput.cpp)
  echo "${command[@]}" 1>&2
  "${command[@]}"
}

run() {
  build_compiler
  init_base
  init_util
}

run "$@"
