#!/usr/bin/env bash

set -u -e

here=$PWD
cd "$(dirname "$0")"

root=$PWD
errors='errors.txt'
compiler="$root/compiler/CompilerCxx/compiler"
compile_cxx=(clang++ -O2 -std=c++11 -o)

standard_src=('standard.0rp' 'standard.0rx')
standard_tm=("$root/standard/standard.0rp")
extra_src=(
  "$root/base/builtin.cpp"
  "$root/base/category-source.cpp"
  "$root/base/logging.cpp"
  "$root/base/types.cpp"
  "$root/capture-thread/src/thread-crosser.cc"
  "$root/standard/Category_BufferedWriter.cpp"
  "$root/standard/Category_LazyStream.cpp"
  "$root/standard/Category_Runner.cpp"
  "$root/standard/Category_SimpleOutput.cpp"
  "$root/standard/Category_Writer.cpp")

init() {
  ghc -i"$root/compiler" "$compiler.hs"
  ( cd "$root/standard" && "$compiler" "${standard_src[@]}" )
}

resolve_files() {
  for f in "$@"; do
    case "$f" in
      /*) ls -d "$f";;
      *)  ls -d "$here/$f";;
    esac
  done
}

general_help() {
  echo "Also see https://github.com/ta0kira/zeolite for more documentation." 1>&2
}

compile() {
  local temp=$1
  local binary_name=$2
  shift 2
  local files=("$@")
  (
    set -e
    cd "$temp" || exit 1
    command0=("$compiler" "${standard_tm[@]}" -- "${files[@]}")
    echo "${command0[@]}" >> "$temp/$errors"
    "${command0[@]}" |& tee -a "$temp/$errors"
    if [[ "${PIPESTATUS[0]}" != 0 ]]; then
      echo "$0: Failed to compile Zeolite sources. See $temp for more details." 1>&2
      general_help
      return 1
    fi
    command1=(
      "${compile_cxx[@]}" "$binary_name"
      -I"$root/capture-thread/include"
      -I"$root/base"
      -I"$root/standard"
      -I"$temp"
      "${extra_src[@]}"
      "$temp"/*cpp)
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

  local all_files=($(resolve_files "$@"))
  local temp=$(mktemp -d)
  local main="$temp/main.cpp"

  init
  create_main "$main" "$main_category"
  compile "$temp" "$here/$main_category" "${all_files[@]}"
  echo "Created binary $main_category." 1>&2
  echo "Also check out intermediate output in $temp." 1>&2
}

run "$@"
