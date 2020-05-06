#!/bin/bash

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

if [ $# -lt 1 ]; then
  echo "$0: Pass the command to execute the zeolite CLI." 1>&2
  exit 1
fi

ZEOLITE=("$@")

execute() {
  echo "Executing:" $(printf ' %q' "$@") 1>&2
  "$@" 2>&1
}

do_zeolite() {
  execute "${ZEOLITE[@]}" "$@"
}

ZEOLITE_PATH=$(do_zeolite --get-path | grep '^/')


test_check_defs() {
  local output=$(do_zeolite -p "$ZEOLITE_PATH" -r tests/check-defs || true)
  if ! echo "$output" | egrep -q "Type .+ is defined 2 times"; then
    echo 'Expected Type definition error from tests/check-defs:' 1>&2
    echo "$output" 1>&2
    return 1
  fi
  if ! echo "$output" | egrep -q "Undefined .+ has not been defined"; then
    echo 'Expected Undefined definition error from tests/check-defs:' 1>&2
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


test_fast() {
  local temp=$(execute mktemp -d)
  local category='HelloWorld'
  local file="$temp/hello-world.0rx"
  cat > "$file" <<END
// $file

concrete $category {
  @type run () -> ()
}

define $category {
  run () {
    \ LazyStream<Formatted>\$new()
        .append("Hello World\n")
        .writeTo(SimpleOutput\$stdout())
  }
}
END
  do_zeolite -i lib/util --fast $category "$file"
  local output=$(execute "./$category")
  if ! echo "$output" | fgrep -xq 'Hello World'; then
    echo 'Expected "Hello World" in program output:' 1>&2
    echo "$output" 1>&2
    return 1
  fi
}


run_all() {
  local failed=0
  for t in "$@"; do
    echo -e "Testing $t >>>\n" 1>&2
    if ! "$t"; then
      failed=1
    fi
    echo -e "\n<<< Testing $t\n" 1>&2
  done
  if (($failed)); then
    echo 'One or more tests failed.' 1>&2
    return 1
  else
    echo 'All tests passed.' 1>&2
  fi
}

run_all test_check_defs test_templates test_fast 1>&2
