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
compiler="$root/compiler/Cli/compiler"

[[ "${COMPILER_CXX-}" ]] || COMPILER_CXX=clang++
[[ "${COMPILE_CXX-}" ]] || COMPILE_CXX=("$COMPILER_CXX" -O2 -std=c++11)

standard_src=('standard.0rp' 'standard.0rx')
standard_tm=(-i "$root/standard/standard.0rp")
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
  (cd "$root/standard" && "$compiler" -c "${standard_src[@]}")
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
  local main="$temp/main.cpp"
  (
    set -e
    cd "$temp" || exit 1
    command0=(
      "$compiler"
      -p "$here"
      -m "$main_category" "$binary_name"
      "${standard_tm[@]}"
      "${files[@]}")
    echo "Compiling Zeolite sources..." 1>&2
    echo "${command0[@]}" >> "$temp/$errors"
    if ! "${command0[@]}" 2> >(tee -a "$temp/$errors" 1>&2); then
      echo "$0: Failed to compile Zeolite sources. See $temp for more details." 1>&2
      general_help
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
  local full_names=$3
  local match_count=$(egrep -c "(^|::)$main_category$" "$full_names")
  if [[ "$match_count" -eq 0 ]]; then
    echo "$0: Invalid main category name '$main_category'" 1>&2
    echo "$0: Failed to compile C++ output. See $temp for more details." 1>&2
    general_help
    exit 1
  fi
  if [[ "$match_count" -gt 1 ]]; then
    echo "$0: Ambiguous main category name '$main_category'" 1>&2
    echo "$0: Failed to compile C++ output. See $temp for more details." 1>&2
    general_help
    exit 1
  fi
  local getter=$(egrep "(^|::)$main_category$" "$full_names" |
                 sed -r 's/^(|[^:]+::)([^:]+)/\1GetType_\2/')
  cat > "$main" <<END
#include "category-source.hpp"

#include "Category_Runner.hpp"
#include "Category_$main_category.hpp"

int main() {
  SetSignalHandler();
  TRACE_FUNCTION("main")
  $getter(T_get()).Call(Function_Runner_run, ParamTuple(), ArgTuple());
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

  init
  compile "$temp" "$here/$main_category" "$main_category" "${all_files[@]}"
  echo "Created binary $main_category." 1>&2
  echo "Also check out intermediate output in $temp." 1>&2
}

run "$@"
