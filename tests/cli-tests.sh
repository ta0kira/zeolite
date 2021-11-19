#!/usr/bin/env bash

# ------------------------------------------------------------------------------
# Copyright 2020-2021 Kevin P. Barry
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

execute_noredir() {
  show_message "Executing:" $(printf ' %q' "$@")
  "$@"
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


require_patterns() {
  local output=$1
  local error=0
  while read pattern; do
    if ! echo "$output" | egrep -q "$pattern"; then
      show_message "Expected pattern \"$pattern\" in output"
      error=1
    fi
  done
  if ((error)); then
    echo "$output" 1>&2
    return 1
  fi
}


exclude_patterns() {
  local output=$1
  local error=0
  while read pattern; do
    if echo "$output" | egrep -q "$pattern"; then
      show_message "Unexpected pattern \"$pattern\" in output"
      error=1
    fi
  done
  if ((error)); then
    echo "$output" 1>&2
    return 1
  fi
}


test_freshness() {
  touch "$ZEOLITE_PATH/tests/freshness"/{,sub1/,sub2/}{public{1,2}.0rp,private{1,2}.0rp,source{1,2}.0rx,test{1,2}.0rt}
  echo '$ModuleOnly$' | tee "$ZEOLITE_PATH/tests/freshness"/{,sub1/,sub2/}private{1,2}.0rp > /dev/null
  echo 'concrete Type {} define Type {}' | tee "$ZEOLITE_PATH/tests/freshness"/{,sub1/,sub2/}private{1,2}.0rx > /dev/null
  rm -f "$ZEOLITE_PATH/tests/freshness"/{,sub1/,sub2/}{public3.0rp,source3.0rx,test3.0rt}

  # Compile and test normally.
  do_zeolite -p "$ZEOLITE_PATH" -R tests/freshness/reverse -f
  do_zeolite -p "$ZEOLITE_PATH" -t tests/freshness/reverse

  # Modify the module's files.
  touch "$ZEOLITE_PATH/tests/freshness"/{,sub1/,sub2/}{public{1,3}.0rp,source{1,3}.0rx,test{1,3}.0rt}
  rm -f "$ZEOLITE_PATH/tests/freshness"/{,sub1/,sub2/}{public2.0rp,private2.0rp,source2.0rx,test2.0rt}
  rm -f "$ZEOLITE_PATH/tests/freshness/.zeolite-cache"/*.so

  local output=$(do_zeolite -p "$ZEOLITE_PATH" -t tests/freshness/reverse || true)
  require_patterns "$output" <<END
tests/freshness/reverse.+out of date
freshness/public1\.0rp.+newer
freshness/public2\.0rp.+missing
freshness/sub1/public1\.0rp.+newer
freshness/sub1/public2\.0rp.+missing
freshness/sub2/public1\.0rp.+newer
freshness/sub2/public2\.0rp.+missing
END
[[ $? -eq 0 ]] || return 1
  exclude_patterns "$output" <<END
private
0rt
0rx
END
[[ $? -eq 0 ]] || return 1

  local output=$(do_zeolite -p "$ZEOLITE_PATH" -t tests/freshness || true)
  require_patterns "$output" <<END
tests/freshness[^/].+out of date
freshness/public3\.0rp.+not present
freshness/source1\.0rx.+newer
freshness/source2\.0rx.+missing
freshness/source3\.0rx.+not present
freshness/sub1/public3\.0rp.+not present
freshness/sub1/source1\.0rx.+newer
freshness/sub1/source2\.0rx.+missing
freshness/sub1/source3\.0rx.+not present
freshness/sub1/test1\.0rt.+newer
freshness/sub1/test2\.0rt.+missing
freshness/sub1/test3\.0rt.+not present
freshness/sub2/public3\.0rp.+not present
freshness/sub2/source1\.0rx.+newer
freshness/sub2/source2\.0rx.+missing
freshness/sub2/source3\.0rx.+not present
freshness/sub2/test1\.0rt.+newer
freshness/sub2/test2\.0rt.+missing
freshness/sub2/test3\.0rt.+not present
freshness/test1\.0rt.+newer
freshness/test2\.0rt.+missing
freshness/test3\.0rt.+not present
freshness/.zeolite-cache/public_[a-f0-9]+\.so.+missing
END
[[ $? -eq 0 ]] || return 1
  exclude_patterns "$output" <<END
cpp
hpp
\.o\b
END
[[ $? -eq 0 ]] || return 1

  do_zeolite -p "$ZEOLITE_PATH" -r tests/freshness

  local output=$(do_zeolite -p "$ZEOLITE_PATH" -t tests/freshness/reverse || true)
  require_patterns "$output" <<END
tests/freshness/reverse.+out of date
freshness/public1\.0rp.+newer
freshness/public3\.0rp.+newer
freshness/sub1/public1\.0rp.+newer
freshness/sub1/public3\.0rp.+newer
freshness/sub2/public1\.0rp.+newer
freshness/sub2/public3\.0rp.+newer
END
[[ $? -eq 0 ]] || return 1
  exclude_patterns "$output" <<END
private
0rt
0rx
END
[[ $? -eq 0 ]] || return 1
}


test_leak_check() {
  local binary="$ZEOLITE_PATH/tests/leak-check/LeakTest"
  rm -f "$binary"
  do_zeolite -p "$ZEOLITE_PATH" -r tests/leak-check -f
  # race-condition check
  # NOTE: If this fails, the valgrind check will be skipped.
  local output=$(execute "$binary" 'race' || true)
  if ! echo "$output" | egrep -q 'no race conditions this time'; then
    show_message 'Unexpected race-condition results from tests/leak-check:'
    echo "$output" 1>&2
    return 1
  fi
  # valgrind check
  if [[ -x "$(which valgrind)" ]]; then
    local output=$(execute valgrind --leak-check=yes "$binary" 'leak' || true)
    if ! echo "$output" | egrep -q 'lost: 0 bytes in 0 blocks'; then
      show_message 'Unexpected valgrind results from tests/leak-check:'
      echo "$output" 1>&2
      return 1
    fi
    if echo "$output" | egrep -q 'lost: [1-9][0-9]* bytes'; then
      show_message 'Unexpected valgrind results from tests/leak-check:'
      echo "$output" 1>&2
      return 1
    fi
  fi
}


test_simulate_refs() {
  do_zeolite -p "$ZEOLITE_PATH" -r tests/simulate-refs -f
}


test_tests_only() {
  local output=$(do_zeolite -p "$ZEOLITE_PATH" -r tests/tests-only -f || true)
  if ! echo "$output" | egrep -q 'Type1 .+ visible category'; then
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


test_tests_only4() {
  local output=$(do_zeolite -p "$ZEOLITE_PATH" -r tests/tests-only4 -f || true)
  if ! echo "$output" | egrep -q 'Type2 .+ \$TestsOnly\$'; then
    show_message 'Expected Type2 import error from tests/tests-only4:'
    echo "$output" 1>&2
    return 1
  fi
  if ! echo "$output" | egrep -q 'In compilation of .+/source2\.cpp'; then
    show_message 'Expected source2.cpp import error from tests/tests-only4:'
    echo "$output" 1>&2
    return 1
  fi
  if echo "$output" | egrep -q 'In compilation of .+/source1\.cpp'; then
    show_message 'Unexpected source1.cpp import error from tests/tests-only4:'
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


test_self_offset() {
  do_zeolite -p "$ZEOLITE_PATH" -r tests/self-offset -f
  do_zeolite -p "$ZEOLITE_PATH" -t tests/self-offset
}


test_templates() {
  execute rm -f $ZEOLITE_PATH/tests/templates/Extension_*.cpp
  do_zeolite -p "$ZEOLITE_PATH" --templates tests/templates
  do_zeolite -p "$ZEOLITE_PATH" -r tests/templates
  do_zeolite -p "$ZEOLITE_PATH" -t tests/templates
}


test_show_deps() {
  local output=$(do_zeolite -p "$ZEOLITE_PATH" --show-deps tests)
  local patterns=(
    'ExprLookup -> Int "/.+/base"'
    'ExprLookup -> String "/.+/base"'
  )
  for pattern in "${patterns[@]}"; do
    if ! echo "$output" | egrep -q "$pattern"; then
      show_message "Expected \"$pattern\" in --show-deps for tests:"
      echo "$output" 1>&2
      return 1
    fi
  done
}


test_fast_static() {
  do_zeolite -p "$ZEOLITE_PATH/tests/fast-static" -I lib/util --fast Program program.0rx
  local output=$(execute "$ZEOLITE_PATH/tests/fast-static/Program")
  if ! echo "$output" | fgrep -xq 'Static linking works!'; then
    show_message 'Expected "Static linking works!" in program output:'
    echo "$output" 1>&2
    return 1
  fi
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
  do_zeolite -p "$ZEOLITE_PATH/example/hello" -I lib/util --fast HelloDemo hello-demo.0rx
  local output=$(echo "$name" | execute_noredir "$binary" 2>&1)
  if ! echo "$output" | egrep -q "\"$name\""; then
    show_message "Expected \"$name\" in HelloDemo output:"
    echo "$output" 1>&2
    return 1
  fi
}


test_example_parser() {
  do_zeolite -p "$ZEOLITE_PATH" -r example/parser -f
  do_zeolite -p "$ZEOLITE_PATH" -t example/parser
}


test_example_primes() {
  local binary="$ZEOLITE_PATH/example/primes/PrimesDemo"
  local expected='2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,103,107,109,113,127,131,137,139,149,151,157,163,167,173,179,181,191,193,197,199,211,223,227,229,233,239,241,251,257,263,269,271,277,281,283,293,307,311,313,317,331,337,347,349,353,359,367,373,379,383,389,397,401,409,419,421,431,433,439,443,449,457,461,463,467,479,487,491,499,503,509,521,523,541,'
  rm -f "$binary"
  local temp=$(execute mktemp)
  do_zeolite -p "$ZEOLITE_PATH" -r example/primes -f
  {
    echo;
    sleep 0.1;
    echo;
    echo "exit";
  } | execute_noredir "$binary" 2> /dev/null | head -n100 | tr $'\n' ',' > "$temp"
  local output=$(cat "$temp")
  rm -f "$temp"
  if [[ "$output" != "$expected" ]]; then
    show_message "Unexpected PrimesDemo output:"
    echo "$output" 1>&2
    return 1
  fi
}


test_example_random() {
  local binary="$ZEOLITE_PATH/example/random/RandomDemo"
  rm -f "$binary"
  local temp=$(execute mktemp)
  do_zeolite -p "$ZEOLITE_PATH" -r example/random -f
  execute_noredir "$binary" 5 > "$temp"
  local output=$(cat "$temp")
  rm -f "$temp"
  if [[ $(echo "$output" | wc -l) -lt 5 ]]; then
    show_message "Expected more output from RandomDemo:"
    echo "$output" 1>&2
    return 1
  fi
}


test_traces() {
  do_zeolite -p "$ZEOLITE_PATH" -r tests/traces -f
  local source_files=("$ZEOLITE_PATH/tests/traces/traces.0rx")
  local expected=$(fgrep -n '// TRACED' "${source_files[@]}" | egrep -o '^[0-9]+' | sort -u)
  local actual=$(do_zeolite -p "$ZEOLITE_PATH" --show-traces "tests/traces" | grep 0rx | sed -r 's/^line ([0-9]+).*/\1/' | sort -u)
  if [[ "$actual" != "$expected" ]]; then
    show_message "Mismatch between expected and actual traced lines:"
    echo "Expected:" $expected 1>&2
    echo "Actual:  " $actual   1>&2
    return 1
  fi
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
  test_bad_system_include
  test_check_defs
  test_example_hello
  test_example_parser
  test_example_primes
  test_example_random
  test_fast_static
  test_freshness
  test_global_include
  test_leak_check
  test_module_only
  test_module_only2
  test_module_only3
  test_module_only4
  test_self_offset
  test_show_deps
  test_simulate_refs
  test_templates
  test_tests_only
  test_tests_only2
  test_tests_only3
  test_tests_only4
  test_traces
  test_warn_public
)

run_all "${ALL_TESTS[@]}" 1>&2
