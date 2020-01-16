#!/usr/bin/env bash

# Copyright 2019-2020 Kevin P. Barry
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

[[ -r ~/.zeoliterc ]] && . ~/.zeoliterc || true

here=$PWD
cd "$(dirname "$0")"

root=$PWD
errors='errors.txt'
compiler="$root/compiler/CompilerCxx/compiler"

[[ "${COMPILER_CXX-}" ]] || COMPILER_CXX=clang++
[[ "${COMPILE_CXX-}" ]] || COMPILE_CXX=("$COMPILER_CXX" -O2 -std=c++11)

standard_src=('standard.0rp' 'standard.0rx')
standard_tm=("$root/standard/standard.0rp")
extra_src=(
  "$root/base/builtin.cpp"
  "$root/base/category-source.cpp"
  "$root/base/logging.cpp"
  "$root/base/types.cpp"
  "$root/capture-thread/src/thread-crosser.cc"
  "$root/standard/Category_BufferedWriter.cpp"
  "$root/standard/Category_IterateForward.cpp"
  "$root/standard/Category_IterateReverse.cpp"
  "$root/standard/Category_LazyStream.cpp"
  "$root/standard/Category_ReadCurrent.cpp"
  "$root/standard/Category_ReadIterator.cpp"
  "$root/standard/Category_Runner.cpp"
  "$root/standard/Category_SimpleOutput.cpp"
  "$root/standard/Category_Writer.cpp")

init() {
  ghc -i"$root/compiler" "$compiler.hs"
  (cd "$root/standard" && "$compiler" "$root" "" "${standard_src[@]}")
}

general_help() {
  echo "Also see https://github.com/ta0kira/zeolite for more documentation." 1>&2
}

compile() {
  local temp=$1
  local binary_name=$2
  local main_category=$3
  shift 3
  local files=("$@")
  (
    set -e
    cd "$temp" || exit 1
    command0=("$compiler" "$root" "$here" "${standard_tm[@]}" -- "${files[@]}")
    echo "Compiling Zeolite sources..." 1>&2
    echo "${command0[@]}" >> "$temp/$errors"
    "${command0[@]}" |& tee -a "$temp/$errors"
    if [[ "${PIPESTATUS[0]}" != 0 ]]; then
      echo "$0: Failed to compile Zeolite sources. See $temp for more details." 1>&2
      general_help
      return 1
    fi
    if [ ! -r "$temp/Category_$main_category.hpp" ]; then
      echo "$0: $main_category has not been defined." 1>&2
      return 1
    fi
    command1=(
      "${COMPILE_CXX[@]}" -o "$binary_name"
      -I"$root/capture-thread/include"
      -I"$root/base"
      -I"$root/standard"
      -I"$temp"
      "${extra_src[@]}"
      "$temp"/*cpp)
    echo "Compiling C++ output..." 1>&2
    echo "${command1[@]}" >> "$temp/$errors"
    "${command1[@]}" |& tee -a "$temp/$errors"
    if [[ "${PIPESTATUS[0]}" != 0 ]]; then
      echo "$0: Failed to compile C++ output. See $temp for more details." 1>&2
      general_help
      return 1
    fi
  )
}

create_main() {
  local main=$1
  local main_category=$2
  cat > "$main" <<END
#include "category-source.hpp"

#include "Category_Runner.hpp"
#include "Category_$main_category.hpp"

int main() {
  SetSignalHandler();
  TRACE_FUNCTION("main")
  GetType_$main_category(T_get()).Call(Function_Runner_run, ParamTuple(), ArgTuple());
}
END
}

run() {
  if [[ $# -lt 2 ]]; then
    echo "$0 [MainCategoryName] [.0rx and .0rp files...]" 1>&2
    general_help
    exit 1
  fi

  local main_category=$1
  shift
  if ! [[ "$main_category" =~ [A-Z][a-z0-9]+ ]]; then
    echo "$0: Invalid main category name '$main_category'" 1>&2
    general_help
    exit 1
  fi

  local all_files=("$@")
  local temp=$(mktemp -d)
  local main="$temp/main.cpp"

  init
  create_main "$main" "$main_category"
  compile "$temp" "$here/$main_category" "$main_category" "${all_files[@]}"
  echo "Created binary $main_category." 1>&2
  echo "Also check out intermediate output in $temp." 1>&2
}

run "$@"
