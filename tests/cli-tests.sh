#!/usr/bin/env bash

# ------------------------------------------------------------------------------
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
# ------------------------------------------------------------------------------

# Author: Kevin P. Barry [ta0kira@gmail.com]

set -e -u

PROGRAM=$(basename "$0")

if [ $# -lt 1 ]; then
  echo "$0: Pass the command to execute the zeolite CLI." 1>&2
  exit 1
fi

ZEOLITE=("$@")

show_message() {
  echo -e "[$PROGRAM]" "$@" 1>&2
}

execute() {
  show_message "Executing:" $(printf ' %q' "$@")
  "$@" 2>&1
}

do_zeolite() {
  execute "${ZEOLITE[@]}" "$@"
}

create_file() {
  show_message "Creating file $1"
  cat > "$1"
}


test_bad_path() {
  local output=$(do_zeolite -p "$ZEOLITE_PATH" -r tests/bad-path -f || true)
  if ! echo "$output" | egrep -q 'bad-path/\.zeolite-module .+ /dev/null'; then
    show_message 'Expected path error from tests/bad-path:'
    echo "$output" 1>&2
    return 1
  fi
}


test_check_defs() {
  local output=$(do_zeolite -p "$ZEOLITE_PATH" -r tests/check-defs -f || true)
  if ! echo "$output" | egrep -q 'Type .+ is defined 2 times'; then
    show_message 'Expected Type definition error from tests/check-defs:'
    echo "$output" 1>&2
    return 1
  fi
  if ! echo "$output" | egrep -q 'Undefined .+ has not been defined'; then
    show_message 'Expected Undefined definition error from tests/check-defs:'
    echo "$output" 1>&2
    return 1
  fi
}


test_tests_only() {
  local output=$(do_zeolite -p "$ZEOLITE_PATH" -r tests/tests-only -f || true)
  if ! echo "$output" | egrep -q 'Definition for Type1 .+ visible category'; then
    show_message 'Expected Type1 definition error from tests/tests-only:'
    echo "$output" 1>&2
    return 1
  fi
  if echo "$output" | egrep -q 'Type2'; then
    show_message 'Unexpected Type2 definition error from tests/tests-only:'
    echo "$output" 1>&2
    return 1
  fi
}


test_tests_only2() {
  local output=$(do_zeolite -p "$ZEOLITE_PATH" -r tests/tests-only2 -f || true)
  if ! echo "$output" | egrep -q 'main.+ Testing'; then
    show_message 'Expected Testing definition error from tests/tests-only:'
    echo "$output" 1>&2
    return 1
  fi
}


test_tests_only3() {
  local output=$(do_zeolite -p "$ZEOLITE_PATH" -r tests/tests-only3 -f || true)
  if ! echo "$output" | egrep -q 'NotTestsOnly.+\$TestsOnly\$'; then
    show_message 'Expected NotTestsOnly definition error from tests/tests-only:'
    echo "$output" 1>&2
    return 1
  fi
}


test_module_only() {
  local output=$(do_zeolite -p "$ZEOLITE_PATH" -R tests/module-only -f || true)
  if ! echo "$output" | egrep -q 'Type1 not found'; then
    show_message 'Expected Type1 definition error from tests/module-only:'
    echo "$output" 1>&2
    return 1
  fi
  if echo "$output" | egrep -q 'Type2 not found'; then
    show_message 'Unexpected Type2 definition error from tests/module-only:'
    echo "$output" 1>&2
    return 1
  fi
}


test_module_only2() {
  local output=$(do_zeolite -p "$ZEOLITE_PATH" -R tests/module-only2 -f || true)
  if ! echo "$output" | egrep -q 'Type1'; then
    show_message 'Expected Type1 definition error from tests/module-only2:'
    echo "$output" 1>&2
    return 1
  fi
  if echo "$output" | egrep -q 'Type2'; then
    show_message 'Unexpected Type2 definition error from tests/module-only2:'
    echo "$output" 1>&2
    return 1
  fi
  if echo "$output" | egrep -q 'private2'; then
    show_message 'Unexpected private2 definition error from tests/module-only2:'
    echo "$output" 1>&2
    return 1
  fi
}


test_module_only3() {
  local output=$(do_zeolite -p "$ZEOLITE_PATH" -R tests/module-only3 -f || true)
  if ! echo "$output" | egrep -q 'Category_Type1\.hpp'; then
    show_message 'Expected Type1 visibility error from tests/module-only3:'
    echo "$output" 1>&2
    return 1
  fi
  if echo "$output" | grep -v 'Writing file' | egrep -q 'Category_Type2\.hpp'; then
    show_message 'Unexpected Type2 visibility error from tests/module-only3:'
    echo "$output" 1>&2
    return 1
  fi
  if echo "$output" | egrep -q 'GetCategory_Type2'; then
    show_message 'Unexpected Type2 visibility error from tests/module-only3:'
    echo "$output" 1>&2
    return 1
  fi
}


test_module_only4() {
  do_zeolite -p "$ZEOLITE_PATH" -r tests/module-only4 -f
  do_zeolite -p "$ZEOLITE_PATH" -t tests/module-only4
}


test_warn_public() {
  local output=$(do_zeolite -p "$ZEOLITE_PATH" -R tests/warn-public -f)
  if ! echo "$output" | egrep -q '"internal" .+ public'; then
    show_message 'Expected "internal" dependency warning from tests/warn-public:'
    echo "$output" 1>&2
    return 1
  fi
  if echo "$output" | egrep -q '"internal2" .+ public'; then
    show_message 'Unexpected "internal2" dependency warning from tests/warn-public:'
    echo "$output" 1>&2
    return 1
  fi
}


test_templates() {
  execute rm -f $ZEOLITE_PATH/tests/templates/Category_Templated.cpp
  do_zeolite -p "$ZEOLITE_PATH" --templates tests/templates
  do_zeolite -p "$ZEOLITE_PATH" -r tests/templates
  do_zeolite -p "$ZEOLITE_PATH" -t tests/templates
}


test_show_deps() {
  local output=$(do_zeolite -p "$ZEOLITE_PATH" --show-deps tests)
  local patterns=(
    'ExprLookup -> Int ".*zeolite/base"'
    'ExprLookup -> String ".*zeolite/base"'
  )
  for pattern in "${patterns[@]}"; do
    if echo "$output" | egrep -q "$pattern"; then
      show_message "Expected \"$pattern\" in --show-deps for tests:"
      echo "$output" 1>&2
      return 1
    fi
  done
}


test_fast() {
  local temp=$(execute mktemp -d)
  local category='HelloWorld'
  local file="$temp/hello-world.0rx"
  create_file "$file" <<END
// $file

concrete $category {
  @type run () -> ()
}

define $category {
  run () {
    \ LazyStream<Formatted>.new()
        .append(\$ExprLookup[MODULE_PATH]\$ + "\n")
        .append("Hello World\n")
        .writeTo(SimpleOutput.stdout())
  }
}
END
  do_zeolite -I lib/util --fast $category "$file"
  local output=$(execute "$PWD/$category")
  if ! echo "$output" | fgrep -xq 'Hello World'; then
    show_message 'Expected "Hello World" in program output:'
    echo "$output" 1>&2
    return 1
  fi
  if ! echo "$output" | fgrep -xq "$PWD"; then
    show_message 'Expected $PWD in program output:'
    echo "$output" 1>&2
    return 1
  fi
  execute rm -r "$temp" "$PWD/$category" || true
}


test_bad_system_include() {
  local temp=$(execute mktemp -d)
  local prev=$PWD
  execute cd "$temp"
  local output=$(do_zeolite -i lib/../tests -c "$temp" || true)
  execute cd "$prev"
  if ! echo "$output" | egrep -q 'Could not find .+tests'; then
    show_message 'Expected error finding "tests":'
    echo "$output" 1>&2
    return 1
  fi
  execute rm -r "$temp" || true
}


test_global_include() {
  local global="$ZEOLITE_PATH/global-paths"
  if [ -f "$global" ]; then
    show_message "Skipping test that overwrites existing $global."
    return 0
  fi
  local temp0=$(execute mktemp -d)
  execute mkdir "$temp0/fakedep"
  do_zeolite -p "$temp0" -c 'fakedep'
  create_file "$global" <<< "$temp0"
  local temp=$(execute mktemp -d)
  local prev=$PWD
  execute cd "$temp"
  do_zeolite -I fakedep -c "$temp" || { execute cd "$prev"; execute rm "$global"; return 1; }
  execute cd "$prev"
  execute rm -r "$temp0" "$temp" "$global" || true
}


test_example_hello() {
  local binary="$ZEOLITE_PATH/example/hello/HelloDemo"
  local name='Cli Tests'
  rm -f "$binary"
  do_zeolite -p "$ZEOLITE_PATH" -I lib/util -m HelloDemo example/hello -f
  local output=$(echo "$name" | "$binary" 2>&1)
  if ! echo "$output" | egrep -q "\"$name\""; then
    show_message "Expected \"$name\" in output:"
    echo "$output" 1>&2
    return 1
  fi
}


test_example_tree() {
  do_zeolite -p "$ZEOLITE_PATH" -I lib/util -m TreeDemo example/tree -f
  do_zeolite -p "$ZEOLITE_PATH" -t example/tree
}


test_example_parser() {
  do_zeolite -p "$ZEOLITE_PATH" -r example/parser -f
  do_zeolite -p "$ZEOLITE_PATH" -t example/parser
}


run_all() {
  ZEOLITE_PATH=$(do_zeolite --get-path | grep '^/')
  echo 1>&2
  local failed=0
  local passed_list=()
  local failed_list=()
  for t in "$@"; do
    show_message "Testing $t >>>"
    echo 1>&2
    if ! "$t"; then
      failed=1
      failed_list=(${failed_list[@]-} "$t")
    else
      passed_list=(${passed_list[@]-} "$t")
    fi
    echo 1>&2
    show_message "<<< Testing $t"
    echo 1>&2
  done
  for t in ${passed_list[@]-}; do
    show_message "*** $t PASSED ***"
  done
  if (($failed)); then
    for t in ${failed_list[@]-}; do
      show_message "*** $t FAILED ***"
    done
    return 1
  else
    show_message 'All tests passed.'
  fi
}

ALL_TESTS=(
  test_bad_path
  test_check_defs
  test_tests_only
  test_tests_only2
  test_tests_only3
  test_module_only
  test_module_only2
  test_module_only3
  test_module_only4
  test_warn_public
  test_templates
  test_show_deps
  test_fast
  test_bad_system_include
  test_global_include
  test_example_hello
  test_example_tree
  test_example_parser
)

run_all "${ALL_TESTS[@]}" 1>&2
