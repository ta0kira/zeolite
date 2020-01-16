#!/bin/bash

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

cd "$(dirname "$0")"

root=$PWD/../..
errors='errors.txt'
main="$PWD/main.cpp"
compiler_hs="$root/compiler/CompilerCxx/compiler.hs"
compiler="$PWD/compiler"

[[ "${COMPILER_CXX-}" ]] || COMPILER_CXX=clang++
[[ "${COMPILE_CXX-}" ]] || COMPILE_CXX=("$COMPILER_CXX" -O0 -g -std=c++11)

standard_src=('standard.0rp' 'standard.0rx')

ghc -i"$root/compiler" "$compiler_hs" -o "$compiler"
(cd "$root/standard" && "$compiler" "$root" "" "${standard_src[@]}")

standard_tm=($root/standard/*.0rp)
standard_inc=($root/standard)
cache_obj=$PWD/.cache
command0=("$compiler" "$root" "" "${standard_tm[@]}" -- /dev/stdin)

compile_src() {
  [[ -d "$cache_obj" ]] || mkdir "$cache_obj"
  (
    cd "$cache_obj"
    local command=(
    "${COMPILE_CXX[@]}" -c
    -I"$root/capture-thread/include"
    -I"$root/base"
    -I"$standard_inc"
    "$@")
    echo "${command[@]}" 1>&2
    "${command[@]}"
  )
}

compile_src "$root/base"/*.cpp
compile_src "$root/standard"/*.cpp
compile_src "$root/capture-thread/src"/*.cc

test_base="
concrete Test {
  defines Runner
}
"

count=0

compile() {
  local temp=$1
  local code=$(cat)
  (
    set -e
    cd "$temp" || exit 1
    echo "${command0[@]}" >> "$temp/$errors"
    { "${command0[@]}" |& tee -a "$temp/$errors"; } < <(echo "$code$test_base")
    [[ "${PIPESTATUS[0]}" = 0 ]] || return 1
    command1=(
      "${COMPILE_CXX[@]}" -o "$temp/compiled"
      -I"$root/capture-thread/include"
      -I"$root/base"
      -I"$temp"
      -I"$standard_inc"
      "$cache_obj"/*.o
      "$temp"/*cpp
      "$main")
    echo "${command1[@]}" >> "$temp/$errors"
    "${command1[@]}" |& tee -a "$temp/$errors"
    [[ "${PIPESTATUS[0]}" = 0 ]] || return 1
  )
}

SKIP_TESTS=${SKIP_TESTS-0}

start_skipping() {
  SKIP_TESTS=1
}

stop_skipping() {
  SKIP_TESTS=0
}

expect_error() {
  ((count++)) || true
  local name=$1
  shift
  if ((SKIP_TESTS)); then
    echo "Test \"$name\" ($count) skipped without compiling or executing" 1>&2
    return
  fi
  local patterns=("$@")
  local temp=$(mktemp -d)
  local code=$(cat)
  (
    set -e
    cd "$temp" || exit 1
    echo "${command0[@]}" >> "$temp/$errors"
    if "${command0[@]}" &>> "$temp/$errors" < <(echo "$code$test_base"); then
      echo "Test \"$name\" ($count): Expected compile error; see output in $temp" 1>&2
      return 1
    fi
  )
  [[ -z "${patterns-}" ]] || for p in "${patterns[@]}"; do
    if ! egrep -q -- "$p" "$temp/$errors"; then
      echo "Test \"$name\" ($count): Expected pattern '$p' in error; see output in $temp" 1>&2
      return 1
    fi
  done
  echo "Test \"$name\" ($count) passed (see output in $temp)" 1>&2
}

expect_runs() {
  ((count++)) || true
  local name=$1
  shift
  if ((SKIP_TESTS)); then
    echo "Test \"$name\" ($count) skipped without compiling or executing" 1>&2
    return
  fi
  local patterns=("$@")
  local temp=$(mktemp -d)
  if ! compile "$temp"; then
    echo "Test \"$name\" ($count): Expected compilation; see output in $temp" 1>&2
    return 1
  fi
  (
    cd "$temp" # Makes sure core dump is in the right place.
    ulimit -Sc unlimited 2> /dev/null || true
    if ! "$temp/compiled" &>> "$temp/$errors"; then
      echo "Test \"$name\" ($count): Expected execution; see output in $temp" 1>&2
      return 1
    fi
  )
  [[ -z "${patterns-}" ]] || for p in "${patterns[@]}"; do
    if ! egrep -q -- "$p" "$temp/$errors"; then
      echo "Test \"$name\" ($count): Expected pattern '$p' in output; see output in $temp" 1>&2
      return 1
    fi
  done
  echo "Test \"$name\" ($count) passed (see output in $temp)" 1>&2
}

expect_crashes() {
  ((count++)) || true
  local name=$1
  shift
  if ((SKIP_TESTS)); then
    echo "Test \"$name\" ($count) skipped without compiling or executing" 1>&2
    return
  fi
  local patterns=("$@")
  local temp=$(mktemp -d)
  if ! compile "$temp"; then
    echo "Test \"$name\" ($count): Expected compilation; see output in $temp" 1>&2
    return 1
  fi
  (
    cd "$temp" # Makes sure core dump is in the right place.
    ulimit -Sc unlimited 2> /dev/null || true
    if "$temp/compiled" &>> "$temp/$errors"; then
      echo "Test \"$name\" ($count): Expected crash; see output in $temp" 1>&2
      return 1
    fi
  )
  [[ -z "${patterns-}" ]] || for p in "${patterns[@]}"; do
    if ! egrep -q -- "$p" "$temp/$errors"; then
      echo "Test \"$name\" ($count): Expected pattern '$p' in error; see output in $temp" 1>&2
      return 1
    fi
  done
  echo "Test \"$name\" ($count) passed (see output in $temp)" 1>&2
}

expect_runs 'do nothing' <<END
define Test {
  run () {}
}
END

expect_crashes 'fail builtin' 'Failed' 'line 4' <<END
define Test {
  @category failedReturn () -> (Int)
  failedReturn () {
    fail("Failed")
  }

  run () {
    Int value <- failedReturn()
  }
}
END

expect_runs 'warning statement after fail' 'line 5' <<END
define Test {
  @category failedReturn () -> (Int)
  failedReturn () {
    fail("Failed")
    return 1
  }

  run () {}
}
END

expect_runs 'warning statement after return' 'line 5' <<END
define Test {
  @category failedReturn () -> (Int)
  failedReturn () {
    return 0
    return 1
  }

  run () {}
}
END

expect_runs 'warning statement after conditional return' 'line 9' <<END
define Test {
  @category failedReturn () -> (Int)
  failedReturn () {
    if (true) {
      return 1
    } else {
      return 2
    }
    return 3
  }

  run () {}
}
END

expect_error 'wrong type for fail' 'fail' 'Formatted' 'line 13' <<END
concrete Value {
  @type create () -> (Value)
}

define Value {
  create () {
    return Value{}
  }
}

define Test {
  run () {
    fail(Value\$create())
  }
}
END

expect_crashes 'fail writer' 'Failed' 'line 3' <<END
define Test {
  run () {
    ~ LazyStream<Formatted>\$new().append("Failed").writeTo(SimpleOutput\$error())
  }
}
END

expect_crashes 'require empty' 'require.+empty' 'line 3' <<END
define Test {
  run () {
    ~ require(empty)
  }
}
END

expect_error '@type member not allowed' 'not allowed' 'line 2' <<END
define Test {
  @type Bool value <- false

  run () {}
}
END

expect_error 'missing member scope' 'scope' 'line 2' <<END
define Test {
  Bool value <- false

  run () {}
}
END

expect_runs '@category member from @type' <<END
define Test {
  @category Bool value <- true

  @type call () -> ()
  call () {
    ~ value
  }

  run () {}
}
END

expect_runs '@category member from @value' <<END
define Test {
  @category Bool value <- true

  @value call () -> ()
  call () {
    ~ value
  }

  run () {}
}
END

expect_error '@category to @category' 'get' 'line 2' <<END
define Test {
  @category Bool value <- get()

  @category get () -> (Bool)
  get () {
    return true
  }

  run () {}
}
END

expect_runs '@category member is lazy' <<END
concrete Util {
  @type doNotUse () -> (Bool)
}

define Util {
  doNotUse () {
    fail("do not use")
  }
}

define Test {
  @category Bool value <- Util\$doNotUse()

  run () {}
}
END

expect_crashes '@category member init when read' 'do not use' 'line 15' <<END
concrete Util {
  @type doNotUse () -> (Bool)
}

define Util {
  doNotUse () {
    fail("do not use")
  }
}

define Test {
  @category Bool value <- Util\$doNotUse()

  run () {
    Bool value2 <- value
  }
}
END

expect_crashes '@category member init when assigned' 'do not use' 'line 15' <<END
concrete Util {
  @type doNotUse () -> (Bool)
}

define Util {
  doNotUse () {
    fail("do not use")
  }
}

define Test {
  @category Bool value <- Util\$doNotUse()

  run () {
    value <- false
  }
}
END

expect_crashes '@category member init when ignored' 'do not use' 'line 15' <<END
concrete Util {
  @type doNotUse () -> (Bool)
}

define Util {
  doNotUse () {
    fail("do not use")
  }
}

define Test {
  @category Bool value <- Util\$doNotUse()

  run () {
    ~ value
  }
}
END

expect_runs '@category member inline assignment' <<END
define Test {
  @category Bool value <- true

  @type call () -> (Bool)
  call () {
    return (value <- false)
  }

  run () {
    if (call() || value) {
      fail("Failed")
    }
  }
}
END

expect_crashes '@category init cycle' 'Value1|Value2' 'line 13|line 21' <<END
concrete Value1 {
  @type get () -> (Bool)
}

concrete Value2 {
  @type get () -> (Bool)
}

define Value1 {
  @category Bool value <- Value2\$get()

  get () {
    return value
  }
}

define Value2 {
  @category Bool value <- Value1\$get()

  get () {
    return value
  }
}

define Test {
  run () {
    ~ Value1\$get()
  }
}
END

expect_error 'self in @category init' 'self' 'line 2' <<END
define Test {
  @category Test value <- self

  run () {}
}
END

expect_error 'self in @category function' 'self' 'line 4' <<END
define Test {
  @category call () -> ()
  call () {
    Test value <- self
  }

  run () {}
}
END

expect_error 'cycle in @category init' 'disallowed' 'line 2' <<END
define Test {
  @category Bool value <- get()

  @category get () -> (Bool)
  get () {
    return value
  }

  run () {}
}
END

expect_runs 'value init in @category member' <<END
define Test {
  @value Int value
  @category Test singleton <- Test{ 3 }

  @value get () -> (Int)
  get () {
    return value
  }

  run () {
    if (singleton.get() != 3) {
      fail(singleton.get())
    }
  }
}
END

expect_runs 'init value same type from @type' <<END
concrete Value<#x> {
  @type create (#x) -> (Value<#x>)
  @value get () -> (#x)
}

define Value {
  @value #x value

  create (val) {
    return Value<#x>{ val }
  }

  get () {
    return value
  }
}

define Test {
  run () {
    Value<Int> value <- Value<Int>\$create(1)
    if (value.get() != 1) {
      fail("Failed")
    }
  }
}
END

expect_runs 'init value different type from @type' <<END
concrete Value<#x> {
  @type create<#y> (#y) -> (Value<#y>)
  @value get () -> (#x)
}

define Value {
  @value #x value

  create (val) {
    return Value<#y>{ val }
  }

  get () {
    return value
  }
}

define Test {
  run () {
    Value<Int> value <- Value<String>\$create<Int>(1)
    if (value.get() != 1) {
      fail("Failed")
    }
  }
}
END

expect_runs 'init value same type from @value' <<END
concrete Value<#x> {
  @type create (#x) -> (Value<#x>)
  @value create2 (#x) -> (Value<#x>)
  @value get () -> (#x)
}

define Value {
  @value #x value

  create (val) {
    return Value<#x>{ val }
  }

  create2 (val) {
    return Value<#x>{ val }
  }

  get () {
    return value
  }
}

define Test {
  run () {
    Value<Int> value <- Value<Int>\$create(2).create2(1)
    if (value.get() != 1) {
      fail("Failed")
    }
  }
}
END

expect_runs 'init value different type from @value' <<END
concrete Value<#x> {
  @type create (#x) -> (Value<#x>)
  @value create2<#y> (#y) -> (Value<#y>)
  @value get () -> (#x)
}

define Value {
  @value #x value

  create (val) {
    return Value<#x>{ val }
  }

  create2 (val) {
    return Value<#y>{ val }
  }

  get () {
    return value
  }
}

define Test {
  run () {
    Value<Int> value <- Value<String>\$create("x").create2<Int>(1)
    if (value.get() != 1) {
      fail("Failed")
    }
  }
}
END

expect_error 'unhandled value return' 'return' 'line 4' <<END
define Test {
  run () {
    // Does't exist, but the error should happen in parsing.
    x.call()
  }
}
END

expect_error 'unhandled type return' 'return' 'line 4' <<END
define Test {
  run () {
    // Does't exist, but the error should happen in parsing.
    X\$call()
  }
}
END

expect_error 'unhandled category return' 'return' 'line 4' <<END
define Test {
  run () {
    // Does't exist, but the error should happen in parsing.
    X\$\$call()
  }
}
END

expect_error 'missing assign' 'value' 'line 5' <<END
@value interface Value {}

define Test {
  @value process () -> (Value)
  process () (value) {}

  run () {}
}
END

expect_runs 'assign before logical' <<END
define Test {
  @value process () -> (Bool)
  process () (value) {
    ~ (value <- true) || false
  }

  run () {}
}
END

expect_error 'assign after logical' 'value' 'line 5' <<END
define Test {
  @value process () -> (Bool)
  process () (value) {
    ~ false || (value <- true)
  }

  run () {}
}
END

expect_runs 'assign before arithmetic' <<END
define Test {
  @value process () -> (Int)
  process () (value) {
    ~ (value <- 1) + 2
  }

  run () {}
}
END

expect_runs 'assign after arithmetic' <<END
define Test {
  @value process () -> (Int)
  process () (value) {
    ~ 2 + (value <- 1)
  }

  run () {}
}
END

expect_error 'return used before assigned' 'value.+initialized' 'line 4' <<END
define Test {
  @category process () -> (Int)
  process () (value) {
    value <- value+1
  }

  run () {}
}
END

expect_runs 'return used after assigned' <<END
define Test {
  @category process () -> (Int)
  process () (value) {
    value <- 1
    value <- value+1
  }

  run () {
    Int value <- process()
    if (value != 2) {
      fail("Failed")
    }
  }
}
END

expect_runs 'returns in correct order' <<END
define Test {
  @type get () -> (Int,Int)
  get () {
    return { 1, 2 }
  }

  run () {
    scoped {
      { Int x, Int y } <- get()
    } in if (x != 1) {
      fail("Failed")
    } elif (y != 2) {
      fail("Failed")
    }
  }
}
END

expect_runs 'assigns in correct order' <<END
concrete Value {
  @type create () -> (Value)
}

define Value {
  create () {
    return Value{}
  }
}

define Test {
  @type get () -> (Value,Int,Int)
  get () (v,x,y) {
    // This makes sure that x and y (primitive) are offset.
    v <- Value\$create()
    x <- 1
    y <- 2
  }

  run () {
    scoped {
      { _, Int x, Int y } <- get()
    } in if (x != 1) {
      fail("Failed")
    } elif (y != 2) {
      fail("Failed")
    }
  }
}
END

expect_runs 'assigns in correct order with explicit return' <<END
define Test {
  @type get () -> (Int,Int)
  get () (x,y) {
    x <- 1
    y <- 2
    return _
  }

  run () {
    scoped {
      { Int x, Int y } <- get()
    } in if (x != 1) {
      fail("Failed")
    } elif (y != 2) {
      fail("Failed")
    }
  }
}
END

expect_error 'overwrite arg' 'arg' 'line 4' <<END
define Test {
  @type call (Int) -> ()
  call (arg) {
    arg <- 2
  }

  run () {}
}
END

expect_error 'missing return' 'Value' 'line 5' <<END
@value interface Value {}

define Test {
  @value process () -> (Value)
  process () {}

  run () {}
}
END

expect_runs 'positional return with no names assigned' <<END
define Test {
  @type get () -> (Int,Int)
  get () (x,y) {
    if (false) {
      x <- 1
    } else {
      return { 3, 4 }
    }
    y <- 2
  }

  run () {
    scoped {
      { Int x, Int y } <- get()
    } in if (x != 3) {
      fail("Failed")
    } elif (y != 4) {
      fail("Failed")
    }
  }
}
END

expect_runs 'positional return instead of names' <<END
define Test {
  @type get () -> (Int,Int)
  get () (x,y) {
    return { 1, 2 }
  }

  run () {
    scoped {
      { Int x, Int y } <- get()
    } in if (x != 1) {
      fail("Failed")
    } elif (y != 2) {
      fail("Failed")
    }
  }
}
END

expect_runs 'positional return with some names assigned' <<END
define Test {
  @type get () -> (Int,Int)
  get () (x,y) {
    y <- 2
    if (false) {
      x <- 1
    } else {
      return { 3, 4 }
    }
  }

  run () {
    scoped {
      { Int x, Int y } <- get()
    } in if (x != 3) {
      fail("Failed")
    } elif (y != 4) {
      fail("Failed")
    }
  }
}
END

expect_runs 'return if/elif/else' <<END
@value interface Value {}

define Test {
  @value process () -> (optional Value)
  process () {
    if (false) {
      return empty
    } elif (false) {
      return empty
    } else {
      return empty
    }
  }

  run () {}
}
END

expect_runs 'assign if/elif/else' <<END
@value interface Value {}

define Test {
  @value process () -> (optional Value)
  process () (value) {
    if (false) {
      value <- empty
    } elif (false) {
      value <- empty
    } else {
      value <- empty
    }
  }

  run () {}
}
END

expect_error 'missing in if' 'value' 'line 12' <<END
@value interface Value {}

define Test {
  @value process () -> (optional Value)
  process () (value) {
    if (false) {
    } elif (false) {
      value <- empty
    } else {
      value <- empty
    }
  }

  run () {}
}
END

expect_error 'missing in elif' 'value' 'line 12' <<END
@value interface Value {}

define Test {
  @value process () -> (optional Value)
  process () (value) {
    if (false) {
      value <- empty
    } elif (false) {
    } else {
      value <- empty
    }
  }

  run () {}
}
END

expect_error 'missing in else' 'value' 'line 12' <<END
@value interface Value {}

define Test {
  @value process () -> (optional Value)
  process () (value) {
    if (false) {
      value <- empty
    } elif (false) {
      value <- empty
    } else {
    }
  }

  run () {}
}
END

expect_error 'missing in implicit else' 'value' 'line 11' <<END
@value interface Value {}

define Test {
  @value process () -> (optional Value)
  process () (value) {
    if (false) {
      value <- empty
    } elif (false) {
      value <- empty
    }
  }

  run () {}
}
END

expect_error 'assign while' 'value' 'line 9' <<END
@value interface Value {}

define Test {
  @value process () -> (optional Value)
  process () (value) {
    while (false) {
      value <- empty
    }
  }

  run () {}
}
END

expect_error 'assign while condition' 'value' 'line 7' <<END
@value interface Value {}

define Test {
  @value process () -> (optional Value)
  process () (value) {
    while (present((value <- empty))) {}
  }

  run () {}
}
END

expect_error 'assign if/elif condition' 'value' 'line 11' <<END
@value interface Value {}

define Test {
  @value process () -> (optional Value)
  process () (value) {
    if (present((value <- empty))) {
    } elif (present((value <- empty))) {
    } else {
      value <- empty
    }
  }

  run () {}
}
END

expect_error 'return while' 'Value' 'line 9' <<END
@value interface Value {}

define Test {
  @value process () -> (optional Value)
  process () {
    while (false) {
      return empty
    }
  }

  run () {}
}
END

expect_runs 'return inside scope' <<END
@value interface Value {}

define Test {
  @value process () -> (optional Value)
  process () {
    scoped {
      return empty
    } in ~ empty
  }

  run () {}
}
END

expect_runs 'return from scoped' <<END
@value interface Value {}

define Test {
  @value process () -> (optional Value)
  process () {
    scoped {
    } in return empty
  }

  run () {}
}
END

expect_error 'break outside of while' 'while' 'line 3' <<END
define Test {
  run () {
    break
  }
}
END

expect_error 'continue outside of while' 'while' 'line 3' <<END
define Test {
  run () {
    continue
  }
}
END

expect_runs 'while with break' <<END
define Test {
  run () {
    Int output <- -1
    scoped {
      Int i <- 0
      Int limit <- 5
    } in while (i < limit) {
      output <- i
      break
      fail("Failed")
    }
    if (output != 0) {
      fail("Failed")
    }
  }
}
END

expect_runs 'while with update' <<END
define Test {
  run () {
    Int output <- -1
    scoped {
      Int i <- 0
      Int limit <- 5
    } in while (i < limit) {
      output <- i
    } update {
      i <- i+1
    }
    if (output != 4) {
      fail("Failed")
    }
  }
}
END

expect_runs 'break in update' <<END
define Test {
  run () {
    Int output <- 0
    while (true) {
    } update {
      if (output > 5) {
        break
        fail("Failed")
      }
      output <- output+1
    }
    if (output != 6) {
      fail("Failed")
    }
  }
}
END

expect_runs 'return in update' <<END
define Test {
  @type test () -> (Int)
  test () {
    Int output <- 0
    while ((output <- output+1) > 0) {
    } update {
      if (output > 5) {
        return output
        fail("Failed")
      }
    }
    return -1
  }

  run () {
    if (test() != 6) {
      fail("Failed")
    }
  }
}
END

expect_runs 'break and continue in if/else' <<END
define Test {
  run () {
    Int i <- 0
    while (true) {
      if (i > 5) {
        break
      } else {
        continue
      }
      fail("Failed")
    } update {
      i <- i+1
    }
    if (i != 6) {
      fail("Failed")
    }
  }
}
END

expect_runs 'update clashes with while' <<END
define Test {
  run () {
    while (false) {
      Int x <- 2
    } update {
      Int x <- 1
    }
  }
}
END

expect_error 'update clashes with scoped' 'x' 'line 7' <<END
define Test {
  run () {
    scoped {
      Int x <- 2
    } in while (false) {
    } update {
      Int x <- 1
    }
  }
}
END

expect_runs 'while without update' <<END
define Test {
  run () {
    Int output <- -1
    scoped {
      Int i <- 0
      Int limit <- 5
    } in while (i < limit) {
      output <- i
      i <- i+1
    }
    if (output != 4) {
      fail("Failed")
    }
  }
}
END

expect_runs 'while with continue and update' <<END
define Test {
  run () {
    Int output <- -1
    scoped {
      Int i <- 0
      Int limit <- 5
    } in while (i < limit) {
      output <- i
      continue
      fail("Failed")
    } update {
      i <- i+1
    }
    if (output != 4) {
      fail("Failed")
    }
  }
}
END

expect_runs 'while with continue and without update' <<END
define Test {
  run () {
    Int output <- -1
    scoped {
      Int i <- 0
      Int limit <- 5
    } in while (i < limit) {
      output <- i
      i <- i+1
      continue
      fail("Failed")
    }
    if (output != 4) {
      fail("Failed")
    }
  }
}
END

expect_crashes 'crash in if' 'empty' 'line 4' <<END
define Test {
  run () {
    optional Bool test <- empty
    if (require(test)) {
      // empty
    }
  }
}
END

expect_crashes 'crash in elif' 'empty' 'line 5' <<END
define Test {
  run () {
    optional Bool test <- empty
    if (false) {
    } elif (require(test)) {
      // empty
    }
  }
}
END

expect_crashes 'crash in while' 'empty' 'line 4' <<END
define Test {
  run () {
    optional Bool test <- empty
    while (require(test)) {
      // empty
    }
  }
}
END

expect_runs 'assign inside scope' <<END
@value interface Value {}

define Test {
  @value process () -> (optional Value)
  process () (value) {
    scoped {
      value <- empty
    } in ~ empty
  }

  run () {}
}
END

expect_runs 'assign from scoped' <<END
@value interface Value {}

define Test {
  @value process () -> (optional Value)
  process () (value) {
    scoped {
    } in value <- empty
  }

  run () {}
}
END

expect_runs 'optional persists' <<END
define Test {
  @value optional Test self2

  @type create () -> (Test)
  create () {
    return Test{ empty }
  }

  @value set () -> ()
  set () {
    scoped {
      Test value <- create()
    } in self2 <- value
  }

  @value check () -> ()
  check () {
    ~ require(self2)
  }

  run () {
    Test value <- create()
    ~ value.set()
    ~ value.check()
  }
}
END

expect_runs 'weak is weak' <<END
define Test {
  @value weak Test self2

  @type create () -> (Test)
  create () {
    return Test{ empty }
  }

  @value set () -> ()
  set () {
    scoped {
      Test value <- create()
    } in self2 <- value
  }

  @value check () -> ()
  check () {
    scoped {
      optional Test self3 <- strong(self2)
    } in if (present(self3)) {
      fail("Failed")
    }
  }

  run () {
    Test value <- create()
    ~ value.set()
    ~ value.check()
  }
}
END

expect_runs 'present weak' <<END
define Test {
  @type create () -> (Test)
  create () {
    return Test{}
  }

  @value check () -> ()
  check () {
    weak Test value <- create()
    if (present(strong(value))) { // value should be nullptr here
      fail("Failed")
    }
  }

  run () {
    Test value <- create()
    ~ value.check()
  }
}
END

expect_runs 'weak variable to weak variable' <<END
define Test {
  @category weak Test one <- empty

  run () {
    weak Test two <- one
    one <- two
  }
}
END

expect_runs 'optional variable to weak variable' <<END
define Test {
  @category optional Test one <- empty

  run () {
    weak Test two <- one
    two <- one
  }
}
END

expect_runs 'weak in multi assign' <<END
concrete Value {
  @type create () -> (Value)
}

define Value {
  create () {
    return Value{}
  }
}

define Test {
  @type get () -> (Value,Value)
  get () {
    Value value <- Value\$create()
    return { value, value }
  }

  run () {
    // value1 ensures value2 is present.
    { Value value1, weak Value value2 } <- get()
    if (!present(strong(value2))) {
      fail("Failed")
    }
  }
}
END

expect_runs 'weak in inline assign' <<END
concrete Value {
  @type create () -> (Value)
}

define Value {
  create () {
    return Value{}
  }
}

define Test {
  run () {
    Value value1 <- Value\$create()
    weak Value value2 <- empty
    if (!present(strong((value2 <- value1)))) {
      fail("Failed")
    }
  }
}
END

expect_runs 'not true' <<END
define Test {
  run () {
    if (!true) {
      fail("Failed")
    }
  }
}
END

expect_runs 'present required' <<END
define Test {
  @type create () -> (Test)
  create () {
    return Test{}
  }

  run () {
    Test value <- create()
    if (!present(value)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'require required' <<END
define Test {
  @type create () -> (Test)
  create () {
    return Test{}
  }

  @value call () -> ()
  call () {}

  run () {
    Test value <- create()
    ~ require(value).call()
  }
}
END

expect_runs 'multi assign if/elif/else' <<END
@value interface Value {}

define Test {
  @value process () -> (optional Value,optional Value)
  process () (value1,value2) {
    if (false) {
      value1 <- empty
      value2 <- empty
    } elif (false) {
      value1 <- empty
      value2 <- empty
    } else {
      value1 <- empty
      value2 <- empty
    }
  }

  run () {}
}
END

expect_error 'multi missing in if' 'value2' 'line 15' <<END
@value interface Value {}

define Test {
  @value process () -> (optional Value,optional Value)
  process () (value1,value2) {
    if (false) {
      value1 <- empty
    } elif (false) {
      value1 <- empty
      value2 <- empty
    } else {
      value1 <- empty
      value2 <- empty
    }
  }

  run () {}
}
END

expect_error 'multi missing in elif' 'value2' 'line 15' <<END
@value interface Value {}

define Test {
  @value process () -> (optional Value,optional Value)
  process () (value1,value2) {
    if (false) {
      value1 <- empty
      value2 <- empty
    } elif (false) {
      value1 <- empty
    } else {
      value1 <- empty
      value2 <- empty
    }
  }

  run () {}
}
END

expect_error 'multi missing in else' 'value2' 'line 15' <<END
@value interface Value {}

define Test {
  @value process () -> (optional Value,optional Value)
  process () (value1,value2) {
    if (false) {
      value1 <- empty
      value2 <- empty
    } elif (false) {
      value1 <- empty
      value2 <- empty
    } else {
      value1 <- empty
    }
  }

  run () {}
}
END

expect_error 'multi return to call' 'call.+\{Value,Value\}' 'line 9' <<END
@value interface Value {
  get () -> (Value,Value)
  call () -> ()
}

define Test {
  @value process (Value) -> ()
  process (value) {
    ~ value.get().call()
  }

  run () {}
}
END

expect_error 'zero return to call' 'call.+\{\}' 'line 9' <<END
@value interface Value {
  get () -> ()
  call () -> ()
}

define Test {
  @value process (Value) -> ()
  process (value) {
    ~ value.get().call()
  }

  run () {}
}
END

expect_runs 'multi return assign' <<END
define Test {
  @type create () -> (Test)
  create () {
    return Test{}
  }

  @value double () -> (Test,Test)
  double () {
    return { self, self }
  }

  run () {
    Test value <- create()
    { _, Test value2 } <- value.double()
    { value, _ } <- value2.double()
  }
}
END

expect_runs 'multi return as args' <<END
define Test {
  @type get () -> (Int,Int)
  get () {
    return { 1, 2 }
  }

  @type call (Int,Int) -> ()
  call (x,y) {
    if (x != 1) {
      fail("Failed")
    }
    if (y != 2) {
      fail("Failed")
    }
  }

  run () {
    ~ call(get())
  }
}
END

expect_runs 'converted call' <<END
@value interface Base {
  call () -> ()
}

concrete Value {
  refines Base

  @type create () -> (Value)
}

define Value {
  call () {}

  create () {
    return Value{}
  }
}

define Test {
  run () {
    Value value <- Value\$create()
    ~ value.Base\$call()
  }
}
END

expect_error 'converted call bad type' 'Base' 'line 21' <<END
@value interface Base {
  call () -> ()
}

concrete Value {
  @value call () -> ()
  @type create () -> (Value)
}

define Value {
  call () {}

  create () {
    return Value{}
  }
}

define Test {
  run () {
    Value value <- Value\$create()
    ~ value.Base\$call()
  }
}
END

expect_error 'call from union' '\[Base\|Value\]' 'line 22' <<END
@value interface Base {
  call () -> ()
}

concrete Value {
  refines Base

  @type create () -> (Value)
}

define Value {
  call () {}

  create () {
    return Value{}
  }
}

define Test {
  run () {
    [Base|Value] value <- Value\$create()
    ~ value.call()
  }
}
END

expect_runs 'call from union with conversion' <<END
@value interface Base {
  call () -> ()
}

concrete Value {
  refines Base

  @type create () -> (Value)
}

define Value {
  call () {}

  create () {
    return Value{}
  }
}

define Test {
  run () {
    [Base|Value] value <- Value\$create()
    ~ value.Base\$call()
  }
}
END

expect_runs 'call from intersect' <<END
@value interface Base1 {
  call () -> ()
}

@value interface Base2 {}

concrete Value {
  refines Base1
  refines Base2

  @type create () -> (Value)
}

define Value {
  call () {}

  create () {
    return Value{}
  }
}

define Test {
  run () {
    [Base1&Base2] value <- Value\$create()
    ~ value.call()
  }
}
END

expect_runs 'call from intersect with conversion' <<END
@value interface Base1 {
  call () -> ()
}

@value interface Base2 {}

concrete Value {
  refines Base1
  refines Base2

  @type create () -> (Value)
}

define Value {
  call () {}

  create () {
    return Value{}
  }
}

define Test {
  run () {
    [Base1&Base2] value <- Value\$create()
    ~ value.Base1\$call()
  }
}
END

expect_runs 'call from intersect with conversion' <<END
@value interface Base1 {
  call () -> ()
}

@value interface Base2 {}

concrete Value {
  refines Base1
  refines Base2

  @type create () -> (Value)
}

define Value {
  call () {}

  create () {
    return Value{}
  }
}

define Test {
  run () {
    [Base1&Base2] value <- Value\$create()
    ~ value.Base1\$call()
  }
}
END

expect_runs 'call from param type' <<END
@type interface Base {
  call () -> ()
}

concrete Value {
  defines Base
}

define Value {
  call () {}
}

define Test {
  @type check<#x>
    #x defines Base
  () -> ()
  check () {
    ~ #x\$call()
  }

  run () {
    ~ check<Value>()
  }
}
END

expect_error 'call from bad param type' 'call.+param #x' 'line 9' <<END
@type interface Base {
  call () -> ()
}

define Test {
  @type check<#x>
  () -> ()
  check () {
    ~ #x\$call()
  }

  run () {}
}
END

expect_runs 'call from param value' <<END
@value interface Base {
  call () -> ()
}

concrete Value {
  refines Base

  @type create () -> (Value)
}

define Value {
  call () {}

  create () {
    return Value{}
  }
}

define Test {
  @type check<#x>
    #x requires Base
  (#x) -> ()
  check (value) {
    ~ value.call()
  }

  run () {
    Value value <- Value\$create()
    ~ check<Value>(value)
  }
}
END

expect_error 'call from bad param value' 'call.+param #x' 'line 9' <<END
@value interface Base {
  call () -> ()
}

define Test {
  @type check<#x>
  (#x) -> ()
  check (value) {
    ~ value.call()
  }

  run () {}
}
END

expect_runs 'convert arg' <<END
@value interface Base {
  call () -> ()
}

concrete Value {
  refines Base

  @type create () -> (Value)
}

define Value {
  call () {}

  create () {
    return Value{}
  }
}

define Test {
  @type convert (Value) -> (Base)
  convert (value) {
    return value
  }

  run () {
   ~ convert(Value\$create()).call()
  }
}
END

expect_error 'bad convert arg' 'does not refine Value' 'line 10' <<END
@value interface Base {}

concrete Value {}

define Value {}

define Test {
  @type convert (Base) -> (Value)
  convert (value) {
    return value
  }

  run () {}
}
END

expect_runs 'external filter not applied in @category' <<END
concrete Value<#x> {
  #x defines LessThan<#x>

  @category something<#x> () -> ()
}

define Value {
  something () {}
}

define Test {
  run () {
    ~ Value\$\$something<Bool>()
  }
}
END

expect_error 'internal param not visible from @type' '#x' 'line 8' <<END
concrete Value {}

define Value {
  types<#x> {}

  @type something () -> ()
  something () {
    optional #x val <- empty
  }
}

define Test {
  run () {}
}
END

expect_runs 'internal filter not applied in @type' <<END
concrete Value {
  @type something<#x> () -> ()
}

define Value {
  types<#x> {
    #x defines LessThan<#x>
  }

  something () {}
}

define Test {
  run () {
    ~ Value\$something<Bool>()
  }
}
END

expect_runs 'internal filter not applied in @category' <<END
concrete Value {
  @category something<#x> () -> ()
}

define Value {
  types<#x> {
    #x defines LessThan<#x>
  }

  something () {}
}

define Test {
  run () {
    ~ Value\$\$something<Bool>()
  }
}
END

expect_runs 'internal params' <<END
concrete Value {
  @type create<#x,#y>
  () -> (Value)
}

define Value {
  types<#x,#y> {}

  create () {
    return Value{ types<#x,#y> }
  }
}

@value interface Type1 {}
@value interface Type2 {}

define Test {
  run () {
    ~ Value\$create<Type1,Type2>()
  }
}
END

expect_runs 'internal params with filters' <<END
@value interface Get<|#x> {
  get () -> (#x)
}

@value interface Set<#x|> {
  set (#x) -> ()
}

concrete Value {
  @type create<#x,#y>
    #x requires Get<#x>
    #y allows Set<#y>
  () -> (Value)
}

define Value {
  types<#x,#y> {
    #x requires Get<#x>
    #y allows Set<#y>
  }

  create () {
    return Value{ types<#x,#y> }
  }
}

define Test {
  run () {}
}
END

expect_error 'internal params missing filters' 'Get|Set' 'line 21' <<END
@value interface Get<|#x> {
  get () -> (#x)
}

@value interface Set<#x|> {
  set (#x) -> ()
}

concrete Value {
  @category create<#x,#y>
  () -> (Value)
}

define Value {
  types<#x,#y> {
    #x requires Get<#x>
    #y allows Set<#y>
  }

  create () {
    return Value{ types<#x,#y> }
  }
}

define Test {
  run () {}
}
END

expect_runs 'internal params with values' <<END
concrete Value {
  @category create<#x,#y>
  () -> (Value)
}

define Value {
  types<#x,#y> {}

  @value Bool value

  create () {
    return Value{ types<#x,#y>, false }
  }
}

define Test {
  run () {}
}
END

expect_runs 'value depends on internal param' <<END
concrete Type<#y> {
  @type create () -> (Type<#y>)
}

define Type {
  create () {
    return Type<#y>{}
  }
}

concrete Value {
  @type create<#x>
  (Type<#x>) -> (Value)
}

define Value {
  types<#z> {}

  @value Type<#z> value

  create (value) {
    return Value{ types<#x>, value }
  }
}

define Test {
  run () {
    ~ Value\$create<Bool>(Type<Bool>\$create())
  }
}
END

expect_error 'value mismatch with internal param' 'call' 'Bool' 'String' 'line 28' <<END
concrete Type<#y> {
  @type create () -> (Type<#y>)
}

define Type {
  create () {
    return Type<#y>{}
  }
}

concrete Value {
  @type create<#x>
  (Type<#x>) -> (Value)
}

define Value {
  types<#z> {}

  @value Type<#z> value

  create (value) {
    return Value{ types<#x>, value }
  }
}

define Test {
  run () {
    ~ Value\$create<String>(Type<Bool>\$create())
  }
}
END

expect_error 'internal param clash with external' '#x' 'line 1' 'line 4' <<END
concrete Value<#x> {}

define Value {
  types<#x> {}
}

define Test {
  run () {}
}
END

expect_error 'internal param clash with function' '#x' 'line 2' 'line 6' <<END
concrete Value {
  @value check<#x> () -> ()
}

define Value {
  types<#x> {}
}

define Test {
  run () {}
}
END

expect_error 'internal param clash with internal function' '#x' 'line 4' 'line 6' <<END
concrete Value {}

define Value {
  types<#x> {}

  @value check<#x> () -> ()
  check () {}
}

define Test {
  run () {}
}
END

expect_runs 'internal param no clash with category' <<END
concrete Value {
  @category create<#x> () -> (Value)
}

define Value {
  types<#x> {}

  create () {
    return Value { types<#x> }
  }
}

define Test {
  run () {}
}
END

expect_runs 'reduce to self' <<END
concrete Value {
  @type create () -> (Value)
}

define Value {
  create () {
    return Value{}
  }
}

define Test {
  run () {
    Value value <- Value\$create()
    scoped {
      optional Value value2 <- reduce<Value,Value>(value)
    } in if (!present(value2)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce to unrelated' <<END
concrete Value {
  @type create () -> (Value)
}

define Value {
  create () {
    return Value{}
  }
}

define Test {
  run () {
    Value value <- Value\$create()
    scoped {
      optional Test value2 <- reduce<Value,Test>(value)
    } in if (present(value2)) {
      fail("Failed")
    }
  }
}
END

expect_error 'reduce wrong arg type' 'argument' 'line 14' <<END
concrete Value {
  @type create () -> (Value)
}

define Value {
  create () {
    return Value{}
  }
}

define Test {
  run () {
    Value value <- Value\$create()
    optional Value value2 <- reduce<Test,Value>(value)
  }
}
END

expect_error 'reduce wrong return type' 'assignment' 'line 14' <<END
concrete Value {
  @type create () -> (Value)
}

define Value {
  create () {
    return Value{}
  }
}

define Test {
  run () {
    Value value <- Value\$create()
    optional Value value2 <- reduce<Value,Test>(value)
  }
}
END

expect_runs 'reduce success with param' <<END
concrete Value<|#x> {
  @type create () -> (Value<#x>)

  @value attempt<#y>
  () -> (optional Value<#y>)
}

define Value {
  create () {
    return Value<#x>{}
  }

  attempt () {
    return reduce<Value<#x>,Value<#y>>(self)
  }
}

@value interface Type1 {}

@value interface Type2 {
  refines Type1
}

define Test {
  run () {
    Value<Type2> value <- Value<Type2>\$create()
    scoped {
      optional Value<Type1> value2 <- value.attempt<Type1>()
    } in if (!present(value2)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce fail with param' <<END
concrete Value<|#x> {
  @type create () -> (Value<#x>)

  @value attempt<#y>
  () -> (optional Value<#y>)
}

define Value {
  create () {
    return Value<#x>{}
  }

  attempt () {
    return reduce<Value<#x>,Value<#y>>(self)
  }
}

@value interface Type1 {}

@value interface Type2 {
  refines Type1
}

define Test {
  run () {
    Value<Type1> value <- Value<Type1>\$create()
    scoped {
      optional Value<Type2> value2 <- value.attempt<Type2>()
    } in if (present(value2)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce success with contra param' <<END
concrete Value<#x|> {
  @type create () -> (Value<#x>)

  @value attempt<#y>
  () -> (optional Value<#y>)
}

define Value {
  create () {
    return Value<#x>{}
  }

  attempt () {
    return reduce<Value<#x>,Value<#y>>(self)
  }
}

@value interface Type1 {}

@value interface Type2 {
  refines Type1
}

define Test {
  run () {
    Value<Value<Type2>> value <- Value<Value<Type2>>\$create()
    scoped {
      optional Value<Value<Type1>> value2 <- value.attempt<Value<Type1>>()
    } in if (!present(value2)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce fail with contra param' <<END
concrete Value<#x|> {
  @type create () -> (Value<#x>)

  @value attempt<#y>
  () -> (optional Value<#y>)
}

define Value {
  create () {
    return Value<#x>{}
  }

  attempt () {
    return reduce<Value<#x>,Value<#y>>(self)
  }
}

@value interface Type1 {}

@value interface Type2 {
  refines Type1
}

define Test {
  run () {
    Value<Value<Type1>> value <- Value<Value<Type1>>\$create()
    scoped {
      optional Value<Value<Type2>> value2 <- value.attempt<Value<Type2>>()
    } in if (present(value2)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce success from union' <<END
@value interface Base {}

concrete Value1 {
  refines Base

  @type create () -> (Value1)
}

define Value1 {
  create () {
    return Value1{}
  }
}

@value interface Value2 {
  refines Base
}

define Test {
  run () {
    [Value1|Value2] value <- Value1\$create()
    scoped {
      optional Base value2 <- reduce<[Value1|Value2],Base>(value)
    } in if (!present(value2)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce fail from union' <<END
@value interface Base {}

concrete Value1 {
  refines Base

  @type create () -> (Value1)
}

define Value1 {
  create () {
    return Value1{}
  }
}

@value interface Value2 {
  refines Base
}

define Test {
  run () {
    [Value1|Value2] value <- Value1\$create()
    scoped {
      optional Value2 value2 <- reduce<[Value1|Value2],Value2>(value)
    } in if (present(value2)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce success to intersect' <<END
@value interface Base1 {}

@value interface Base2 {}

concrete Value {
  refines Base1
  refines Base2

  @type create () -> (Value)
}

define Value {
  create () {
    return Value{}
  }
}

define Test {
  run () {
    Value value <- Value\$create()
    scoped {
      optional [Base1&Base2] value2 <- reduce<Value,[Base1&Base2]>(value)
    } in if (!present(value2)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce fail to intersect' <<END
@value interface Base1 {}

@value interface Base2 {}

concrete Value {
  refines Base1

  @type create () -> (Value)
}

define Value {
  create () {
    return Value{}
  }
}

define Test {
  run () {
    Value value <- Value\$create()
    scoped {
      optional [Base1&Base2] value2 <- reduce<Value,[Base1&Base2]>(value)
    } in if (present(value2)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce success union to intersect' <<END
@value interface Base1 {}

@value interface Base2 {}

@value interface Value1 {
  refines Base1
  refines Base2
}

concrete Value2 {
  refines Base1
  refines Base2

  @type create () -> (Value2)
}

define Value2 {
  create () {
    return Value2{}
  }
}

define Test {
  run () {
    [Value1|Value2] value <- Value2\$create()
    scoped {
      optional [Base1&Base2] value2 <- reduce<[Value1|Value2],[Base1&Base2]>(value)
    } in if (!present(value2)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce fail union to intersect' <<END
@value interface Base1 {}

@value interface Base2 {}

@value interface Value1 {
  refines Base1
  refines Base2
}

concrete Value2 {
  refines Base1

  @type create () -> (Value2)
}

define Value2 {
  create () {
    return Value2{}
  }
}

define Test {
  run () {
    [Value1|Value2] value <- Value2\$create()
    scoped {
      optional [Base1&Base2] value2 <- reduce<[Value1|Value2],[Base1&Base2]>(value)
    } in if (present(value2)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce success intersect to union' <<END
@value interface Base1 {}

@value interface Base2 {}

@value interface Value1 {
  refines Base1
}

@value interface Value2 {}

concrete Data {
  refines Value1
  refines Value2

  @type create () -> (Data)
}

define Data {
  create () {
    return Data{}
  }
}

define Test {
  run () {
    [Value1&Value2] value <- Data\$create()
    scoped {
      optional [Base1|Base2] value2 <- reduce<[Value1&Value2],[Base1|Base2]>(value)
    } in if (!present(value2)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce fail intersect to union' <<END
@value interface Base1 {}

@value interface Base2 {}

@value interface Value1 {}

@value interface Value2 {}

concrete Data {
  refines Value1
  refines Value2

  @type create () -> (Data)
}

define Data {
  create () {
    return Data{}
  }
}

define Test {
  run () {
    [Value1&Value2] value <- Data\$create()
    scoped {
      optional [Base1|Base2] value2 <- reduce<[Value1&Value2],[Base1|Base2]>(value)
    } in if (present(value2)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce succeeds to covariant any' <<END
concrete Value<|#x> {
  @type create () -> (Value<#x>)
}

define Value {
  create () {
    return Value<#x>{}
  }
}

define Test {
  run () {
    Value<Test> value <- Value<Test>\$create()
    scoped {
      optional Value<any> value2 <- reduce<Value<Test>,Value<any>>(value)
    } in if (!present(value2)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce succeeds to contravariant all' <<END
concrete Value<#x|> {
  @type create () -> (Value<#x>)
}

define Value {
  create () {
    return Value<#x>{}
  }
}

define Test {
  run () {
    Value<Test> value <- Value<Test>\$create()
    scoped {
      optional Value<all> value2 <- reduce<Value<Test>,Value<all>>(value)
    } in if (!present(value2)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce fails to invariant any' <<END
concrete Value<#x> {
  @type create () -> (Value<#x>)
}

define Value {
  create () {
    return Value<#x>{}
  }
}

define Test {
  run () {
    Value<Test> value <- Value<Test>\$create()
    scoped {
      optional Value<any> value2 <- reduce<Value<Test>,Value<any>>(value)
    } in if (present(value2)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce succeeds from covariant all' <<END
concrete Value<|#x> {
  @type create () -> (Value<#x>)
}

define Value {
  create () {
    return Value<#x>{}
  }
}

define Test {
  run () {
    Value<all> value <- Value<all>\$create()
    scoped {
      optional Value<Test> value2 <- reduce<Value<all>,Value<Test>>(value)
    } in if (!present(value2)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce succeeds from contravariant any' <<END
concrete Value<#x|> {
  @type create () -> (Value<#x>)
}

define Value {
  create () {
    return Value<#x>{}
  }
}

define Test {
  run () {
    Value<any> value <- Value<any>\$create()
    scoped {
      optional Value<Test> value2 <- reduce<Value<any>,Value<Test>>(value)
    } in if (!present(value2)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce fails from invariant all' <<END
concrete Value<#x> {
  @type create () -> (Value<#x>)
}

define Value {
  create () {
    return Value<#x>{}
  }
}

define Test {
  run () {
    Value<all> value <- Value<all>\$create()
    scoped {
      optional Value<Test> value2 <- reduce<Value<all>,Value<Test>>(value)
    } in if (present(value2)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce Bool to Bool' <<END
define Test {
  run () {
    if (!present(reduce<Bool,Bool>(true))) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce Bool to Formatted' <<END
define Test {
  run () {
    if (!present(reduce<Bool,Formatted>(true))) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce Int to Int' <<END
define Test {
  run () {
    if (!present(reduce<Int,Int>(1))) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce Int to Formatted' <<END
define Test {
  run () {
    if (!present(reduce<Int,Formatted>(1))) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce String to String' <<END
define Test {
  run () {
    if (!present(reduce<String,String>("x"))) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce String to Formatted' <<END
define Test {
  run () {
    if (!present(reduce<String,Formatted>("x"))) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce Float to Float' <<END
define Test {
  run () {
    if (!present(reduce<Float,Float>(1.1))) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce Float to Formatted' <<END
define Test {
  run () {
    if (!present(reduce<Float,Formatted>(1.1))) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce ReadPosition success' <<END
define Test {
  run () {
    if (!present(reduce<ReadPosition<Char>,ReadPosition<Formatted>>("x"))) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce ReadPosition fail' <<END
define Test {
  run () {
    if (present(reduce<ReadPosition<Formatted>,ReadPosition<Char>>("x"))) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce internal param success' <<END
concrete Value {
  @type create<#x> () -> (Value)
  @value check<#y> (#y) -> (Bool)
}

define Value {
  types<#x> {}

  create () {
    return Value { types<#x> }
  }

  check (y) {
    return present(reduce<#y,#x>(y))
  }
}

define Test {
  run () {
    Value value <- Value\$create<Formatted>()
    if (!value.check<String>("")) {
      fail("Failed")
    }
  }
}
END

expect_runs 'reduce internal param fail' <<END
concrete Value {
  @type create<#x> () -> (Value)
  @value check<#y> (#y) -> (Bool)
}

define Value {
  types<#x> {}

  create () {
    return Value { types<#x> }
  }

  check (y) {
    return present(reduce<#y,#x>(y))
  }
}

define Test {
  run () {
    Value value <- Value\$create<Formatted>()
    if (value.check<Value>(value)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'int arithmetic with precedence' <<END
define Test {
  run () {
    scoped {
      Int x <- 0x10 + 1 * 2 - 8 / 2 - 3 % 2
    } in if (x != 13) {
      fail("Failed")
    }
  }
}
END

expect_error 'int + bool' 'Int.+Bool' 'line 3' <<END
define Test {
  run () {
    ~ 0x10 + false
  }
}
END

expect_error 'int + string' 'Int.+String' 'line 3' <<END
define Test {
  run () {
    ~ 0x10 + ""
  }
}
END

expect_runs 'string arithmetic' <<END
define Test {
  run () {
    scoped {
      String x <- "x" + "y" + "z"
    } in if (x != "xyz") {
      fail("Failed")
    }
  }
}
END

expect_runs 'float arithmetic with precedence' <<END
define Test {
  run () {
    scoped {
      Float x <- 16.0 + 1.0 * 2.0 - 8.0 / 2.0 - 3.0 / 3.0
    } in if (x != 13.0) {
      fail("Failed")
    }
  }
}
END

expect_runs 'bool comparison' <<END
define Test {
  run () {
    if (!(true  == true))  { fail("Failed") }
    if (!(false == false)) { fail("Failed") }
    if (!(false != true))  { fail("Failed") }
  }
}
END

expect_runs 'int comparison' <<END
define Test {
  run () {
    if (!(1 <  2)) { fail("Failed") }
    if (!(1 <= 2)) { fail("Failed") }
    if (!(1 == 1)) { fail("Failed") }
    if (!(1 != 2)) { fail("Failed") }
    if (!(2 >  1)) { fail("Failed") }
    if (!(2 >= 1)) { fail("Failed") }
  }
}
END

expect_runs 'float comparison' <<END
define Test {
  run () {
    if (!(1.0 <  2.0)) { fail("Failed") }
    if (!(1.0 <= 2.0)) { fail("Failed") }
    if (!(1.0 == 1.0)) { fail("Failed") }
    if (!(1.0 != 2.0)) { fail("Failed") }
    if (!(2.0 >  1.0)) { fail("Failed") }
    if (!(2.0 >= 1.0)) { fail("Failed") }
  }
}
END

expect_runs 'string comparison' <<END
define Test {
  run () {
    if (!("x" <  "y")) { fail("Failed") }
    if (!("x" <= "y")) { fail("Failed") }
    if (!("x" == "x")) { fail("Failed") }
    if (!("x" != "y")) { fail("Failed") }
    if (!("y" >  "x")) { fail("Failed") }
    if (!("y" >= "x")) { fail("Failed") }
  }
}
END

expect_runs 'char comparison' <<END
define Test {
  run () {
    if (!('x' <  'y')) { fail("Failed") }
    if (!('x' <= 'y')) { fail("Failed") }
    if (!('x' == 'x')) { fail("Failed") }
    if (!('x' != 'y')) { fail("Failed") }
    if (!('y' >  'x')) { fail("Failed") }
    if (!('y' >= 'x')) { fail("Failed") }
  }
}
END

expect_runs 'bool logic with precedence' <<END
define Test {
  run () {
    scoped {
      Bool x <- false && false || true
    } in if (!x) {
      fail("Failed")
    }
  }
}
END

expect_error 'minus string' 'String.+String' 'line 3' <<END
define Test {
  run () {
    ~ "x" - "x"
  }
}
END

expect_error 'arithmetic bool' 'Bool.+Bool' 'line 3' <<END
define Test {
  run () {
    ~ true - false
  }
}
END

expect_runs 'string plus with comparison' <<END
define Test {
  run () {
    if (!("x" + "w" < "x" + "y")) {
      fail("Failed")
    }
  }
}
END

expect_runs 'char minus with comparison' <<END
define Test {
  run () {
    if (!('d' - 'a' == 3)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'int arithmetic with comparison' <<END
define Test {
  run () {
    if (!(2 + 1 < 2 + 3)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'float arithmetic with comparison' <<END
define Test {
  run () {
    if (!(2.0 + 1.0 < 2.0 + 3.0)) {
      fail("Failed")
    }
  }
}
END

expect_error 'bool comparison' 'Bool.+Int' 'line 3' <<END
define Test {
  run () {
    ~ 1 < 2 < 3
  }
}
END

expect_runs 'arithmetic, comparison, logic' <<END
define Test {
  run () {
    scoped {
      Bool x <- 1 + 2 < 4 && 3 >= 1 * 2 + 1
    } in if (!x) {
      fail("Failed")
    }
  }
}
END

expect_runs 'string LessThan' <<END
define Test {
  run () {
    if (!String\$lessThan("x","y")) {
      fail("Failed")
    }
  }
}
END

expect_runs 'bool Equals' <<END
define Test {
  run () {
    if (!Bool\$equals(true,true)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'string Equals' <<END
define Test {
  run () {
    if (!String\$equals("x","x")) {
      fail("Failed")
    }
  }
}
END

expect_runs 'char LessThan' <<END
define Test {
  run () {
    if (!Char\$lessThan('x','y')) {
      fail("Failed")
    }
  }
}
END

expect_runs 'char Equals' <<END
define Test {
  run () {
    if (!Char\$equals('x','x')) {
      fail("Failed")
    }
  }
}
END

expect_runs 'int LessThan' <<END
define Test {
  run () {
    if (!Int\$lessThan(1,2)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'int Equals' <<END
define Test {
  run () {
    if (!Int\$equals(1,1)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'float LessThan' <<END
define Test {
  run () {
    if (!Float\$lessThan(1.0,2.0)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'float Equals' <<END
define Test {
  run () {
    if (!Float\$equals(1.0,1.0)) {
      fail("Failed")
    }
  }
}
END

expect_runs 'bool is shared' <<END
define Test {
  run () {
    Bool value1 <- true
    // Shared because true and false are boxed constants.
    weak Bool value2 <- value1
    if (!present(strong(value2))) {
      fail("Failed")
    }
  }
}
END

expect_runs 'string is shared' <<END
define Test {
  run () {
    String value1 <- "x"
    weak String value2 <- value1
    if (!present(strong(value2))) {
      fail("Failed")
    }
  }
}
END

expect_runs 'char is not shared' <<END
define Test {
  run () {
    Char value1 <- 'x'
    weak Char value2 <- value1
    if (present(strong(value2))) {
      fail("Failed")
    }
  }
}
END

expect_runs 'int is not shared' <<END
define Test {
  run () {
    Int value1 <- 1
    weak Int value2 <- value1
    if (present(strong(value2))) {
      fail("Failed")
    }
  }
}
END

expect_runs 'float is not shared' <<END
define Test {
  run () {
    Float value1 <- 1.1
    weak Float value2 <- value1
    if (present(strong(value2))) {
      fail("Failed")
    }
  }
}
END

expect_runs 'internal merge' <<END
@value interface Interface {
  call () -> (Interface)
}

concrete Value {
  refines Interface
  @type create () -> (Value)
}

define Value {
  create () {
    return Value{}
  }

  @value call () -> (Value)
  call () {
    return self
  }
}

define Test {
  run () {
    ~ Value\$create().call().call()
  }
}
END

expect_error 'internal merge failed' 'Interface2' 'line 18' <<END
@value interface Interface {}

@value interface Interface2 {
  refines Interface
  call () -> (Interface2)
}

concrete Value {
  refines Interface2
  @type create () -> (Value)
}

define Value {
  create () {
    return Value{}
  }

  @value call () -> (Interface)
  call () {
    return self
  }
}

define Test {
  run () {}
}
END

expect_runs 'external merge' <<END
@value interface Interface {
  call () -> (Interface)
}

concrete Value {
  refines Interface
  @type create () -> (Value)
  @value call () -> (Value)
}

define Value {
  create () {
    return Value{}
  }

  call () {
    return self
  }
}

define Test {
  run () {
    ~ Value\$create().call().call()
  }
}
END

expect_error 'external merge failed' 'Interface2' 'line 11' <<END
@value interface Interface {}

@value interface Interface2 {
  refines Interface
  call () -> (Interface2)
}

concrete Value {
  refines Interface2
  @type create () -> (Value)
  @value call () -> (Interface)
}

define Test {
  run () {}
}
END

expect_runs 'string access' <<END
define Test {
  run () {
    String s <- "abcde"
    Char c <- s.readPosition(3)
    if (c != 'd') {
      fail(c)
    }
    Int size <- s.readSize()
    if (size != 5) {
      fail(size)
    }
  }
}
END

expect_crashes 'string access negative' '-10' 'bounds' 'line 3' <<END
define Test {
  run () {
    ~ ("abc").readPosition(-10)
  }
}
END

expect_crashes 'string access past end' '100' 'bounds' 'line 3' <<END
define Test {
  run () {
    ~ ("abc").readPosition(100)
  }
}
END

expect_runs 'string Formatted' <<END
define Test {
  run () {
    String s <- ("x").formatted()
    if (s != "x") {
      fail(s)
    }
  }
}
END

expect_runs 'char Formatted' <<END
define Test {
  run () {
    String s <- ('x').formatted()
    if (s != "x") {
      fail(s)
    }
  }
}
END

expect_runs 'char octal Formatted' <<END
define Test {
  run () {
    String s <- ('\170').formatted()
    if (s != "x") {
      fail(s)
    }
  }
}
END

expect_runs 'char hex Formatted' <<END
define Test {
  run () {
    String s <- ('\x78').formatted()
    if (s != "x") {
      fail(s)
    }
  }
}
END

expect_runs 'int Formatted' <<END
define Test {
  run () {
    String s <- (1).formatted()
    if (s != "1") {
      fail(s)
    }
  }
}
END

expect_runs 'int hex Formatted' <<END
define Test {
  run () {
    String s <- (0x0010).formatted()
    if (s != "16") {
      fail(s)
    }
  }
}
END

expect_runs 'float Formatted' <<END
define Test {
  run () {
    String s <- (1.1).formatted()
    if (s != "1.1") { // precision might vary
      fail(s)
    }
  }
}
END

expect_runs 'bool Formatted' <<END
define Test {
  run () {
    String s <- (false).formatted()
    if (s != "false") {
      fail(s)
    }
  }
}
END

expect_runs 'string typename' <<END
define Test {
  run () {
    Formatted name <- typename<String>()
    if (name.formatted() != "String") {
      fail(name)
    }
  }
}
END

expect_runs 'int typename' <<END
define Test {
  run () {
    Formatted name <- typename<Int>()
    if (name.formatted() != "Int") {
      fail(name)
    }
  }
}
END

expect_runs 'float typename' <<END
define Test {
  run () {
    Formatted name <- typename<Float>()
    if (name.formatted() != "Float") {
      fail(name)
    }
  }
}
END

expect_runs 'bool typename' <<END
define Test {
  run () {
    Formatted name <- typename<Bool>()
    if (name.formatted() != "Bool") {
      fail(name)
    }
  }
}
END

expect_runs 'Formatted typename' <<END
define Test {
  run () {
    Formatted name <- typename<Formatted>()
    if (name.formatted() != "Formatted") {
      fail(name)
    }
  }
}
END

expect_runs 'LessThan typename' <<END
define Test {
  run () {
    Formatted name <- typename<LessThan<Int>>()
    if (name.formatted() != "LessThan<Int>") {
      fail(name)
    }
  }
}
END

expect_runs 'Equals typename' <<END
define Test {
  run () {
    Formatted name <- typename<Equals<Int>>()
    if (name.formatted() != "Equals<Int>") {
      fail(name)
    }
  }
}
END

expect_runs 'any typename' <<END
define Test {
  run () {
    Formatted name <- typename<any>()
    if (name.formatted() != "any") {
      fail(name)
    }
  }
}
END

expect_runs 'all typename' <<END
define Test {
  run () {
    Formatted name <- typename<all>()
    if (name.formatted() != "all") {
      fail(name)
    }
  }
}
END

expect_runs 'intersect typename' <<END
define Test {
  run () {
    Formatted name <- typename<[String&Int]>()
    if (name.formatted() != "[String&Int]") {
      fail(name)
    }
  }
}
END

expect_runs 'union typename' <<END
define Test {
  run () {
    Formatted name <- typename<[String|Int]>()
    if (name.formatted() != "[String|Int]") {
      fail(name)
    }
  }
}
END

expect_runs 'param typename' <<END
@value interface Type<#x,#y> {}

define Test {
  @category getTypename<#x,#y> () -> (Formatted)
  getTypename () {
    return typename<Type<#x,#y>>()
  }

  run () {
    Formatted name <- getTypename<String,LessThan<Int>>()
    if (name.formatted() != "Type<String,LessThan<Int>>") {
      fail(name)
    }
  }
}
END

expect_runs 'iterate string forward' <<END
define Test {
  run () {
    String s <- "abcde"
    Int count <- 0
    scoped {
      ReadIterator<Char> iter <- ReadIterator\$\$fromReadPosition<Char>(s)
    } in while (!iter.pastForwardEnd()) {
      if (iter.readCurrent() != s.readPosition(count)) {
        fail(iter.readCurrent())
      }
    } update {
      count <- count+1
      iter <- iter.forward()
    }
    if (count != s.readSize()) {
     fail(count)
    }
  }
}
END

expect_runs 'iterate string reverse' <<END
define Test {
  run () {
    String s <- "abcde"
    Int count <- s.readSize()-1
    scoped {
      ReadIterator<Char> iter <- ReadIterator\$\$fromReadPositionAt<Char>(s,count)
    } in while (!iter.pastReverseEnd()) {
      if (iter.readCurrent() != s.readPosition(count)) {
        fail(iter.readCurrent())
      }
    } update {
      count <- count-1
      iter <- iter.reverse()
    }
    if (count != -1) {
     fail(count)
    }
  }
}
END

echo "All $count tests passed" 1>&2
