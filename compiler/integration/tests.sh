#!/bin/bash

set -u -e

cd "$(dirname "$0")"

root=$PWD/../..
errors='errors.txt'
main="$PWD/main.cpp"
compiler="$root/compiler/CompilerCxx/compiler"

ghc -i"$root/compiler" "$compiler.hs"

test_base="
@type interface Runner {
  run () -> ()
}
concrete Util {
  @type crash () -> ()
}
define Util {
  crash () {
    ~ require(empty)
  }
}
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
    { "$compiler" /dev/stdin |& tee -a "$temp/$errors"; } < <(echo "$code$test_base")
    [[ "${PIPESTATUS[0]}" = 0 ]] || return 1
    command=(
      clang++ -O0 -g -std=c++11 -o "$temp/compiled"
      -I"$root/capture-thread/include"
      -I"$root/base"
      -I"$temp"
      "$root/capture-thread/src"/*.cc
      "$root/base"/*cpp
      "$temp"/*cpp
      "$main")
    echo "${command[@]}" >> "$temp/$errors"
    "${command[@]}" |& tee -a "$temp/$errors"
    [[ "${PIPESTATUS[0]}" = 0 ]] || return 1
  )
}

expect_error() {
  ((count++)) || true
  local name=$1
  shift
  local patterns=("$@")
  local temp=$(mktemp -d)
  local code=$(cat)
  (
    set -e
    cd "$temp" || exit 1
    if "$compiler" /dev/stdin &> "$temp/$errors" < <(echo "$code$test_base"); then
      echo "Test \"$name\" ($count): Expected compile error; see output in $temp" 1>&2
      return 1
    fi
  )
  for p in "${patterns[@]}"; do
    if ! egrep -q "$p" "$temp/$errors"; then
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
  local patterns=("$@")
  local temp=$(mktemp -d)
  if ! compile "$temp"; then
    echo "Test \"$name\" ($count): Expected compilation; see output in $temp" 1>&2
    return 1
  fi
  ulimit -Sc unlimited 2> /dev/null || true
  "$temp/compiled" |& tee -a "$temp/$errors"
  if [[ "${PIPESTATUS[0]}" != 0 ]]; then
    echo "Test \"$name\" ($count): Expected execution; see output in $temp" 1>&2
    return 1
  fi
  for p in "${patterns[@]}"; do
    if ! egrep -q "$p" "$temp/$errors"; then
      echo "Test \"$name\" ($count): Expected pattern '$p' in error; see output in $temp" 1>&2
      return 1
    fi
  done
  echo "Test \"$name\" ($count) passed (see output in $temp)" 1>&2
}

expect_crashes() {
  ((count++)) || true
  local name=$1
  local temp=$(mktemp -d)
  if ! compile "$temp"; then
    echo "Test \"$name\" ($count): Expected compilation; see output in $temp" 1>&2
    return 1
  fi
  ulimit -Sc 0 2> /dev/null || true
  if "$temp/compiled" &> "$temp/$errors"; then
    echo "Test \"$name\" ($count): Expected crash; see output in $temp" 1>&2
    return 1
  fi
  echo "Test \"$name\" ($count) passed (see output in $temp)" 1>&2
}

expect_runs 'do nothing' <<END
define Test {
  run () {}
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

expect_error '@value init in @category init' 'allowed' 'line 2' <<END
define Test {
  @category Test value <- Test{}

  run () {}
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

expect_error 'missing return' 'Value' 'line 5' <<END
@value interface Value {}

define Test {
  @value process () -> (Value)
  process () {}

  run () {}
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

expect_error 'return with name' 'return' 'line 6' <<END
@value interface Value {}

define Test {
  @value process () -> (optional Value)
  process () (value) {
    return empty
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
      ~ Util\$crash() // force a crash if present
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
      ~ Util\$crash() // force a crash if present
    }
  }

  run () {
    Test value <- create()
    ~ value.check()
  }
}
END

expect_runs 'not true' <<END
define Test {
  run () {
    if (!true) {
      ~ Util\$crash()
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
      ~ Util\$crash() // force a crash if empty
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

expect_error 'multi return to call' 'call.+found 2' 'line 9' <<END
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

expect_error 'zero return to call' 'call.+found 0' 'line 9' <<END
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

expect_error 'call from union' '\(Base\|Value\)' 'line 22' <<END
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
    (Base|Value) value <- Value\$create()
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
    (Base|Value) value <- Value\$create()
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
    (Base1&Base2) value <- Value\$create()
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
    (Base1&Base2) value <- Value\$create()
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
  @type create<#x,#y>
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
  @type create<#x,#y>
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
      ~ Util\$crash()
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
      ~ Util\$crash()
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
      ~ Util\$crash()
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
      ~ Util\$crash()
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
      ~ Util\$crash()
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
      ~ Util\$crash()
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
    (Value1|Value2) value <- Value1\$create()
    scoped {
      optional Base value2 <- reduce<(Value1|Value2),Base>(value)
    } in if (!present(value2)) {
      ~ Util\$crash()
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
    (Value1|Value2) value <- Value1\$create()
    scoped {
      optional Value2 value2 <- reduce<(Value1|Value2),Value2>(value)
    } in if (present(value2)) {
      ~ Util\$crash()
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
      optional (Base1&Base2) value2 <- reduce<Value,(Base1&Base2)>(value)
    } in if (!present(value2)) {
      ~ Util\$crash()
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
      optional (Base1&Base2) value2 <- reduce<Value,(Base1&Base2)>(value)
    } in if (present(value2)) {
      ~ Util\$crash()
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
    (Value1|Value2) value <- Value2\$create()
    scoped {
      optional (Base1&Base2) value2 <- reduce<(Value1|Value2),(Base1&Base2)>(value)
    } in if (!present(value2)) {
      ~ Util\$crash()
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
    (Value1|Value2) value <- Value2\$create()
    scoped {
      optional (Base1&Base2) value2 <- reduce<(Value1|Value2),(Base1&Base2)>(value)
    } in if (present(value2)) {
      ~ Util\$crash()
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
    (Value1&Value2) value <- Data\$create()
    scoped {
      optional (Base1|Base2) value2 <- reduce<(Value1&Value2),(Base1|Base2)>(value)
    } in if (!present(value2)) {
      ~ Util\$crash()
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
    (Value1&Value2) value <- Data\$create()
    scoped {
      optional (Base1|Base2) value2 <- reduce<(Value1&Value2),(Base1|Base2)>(value)
    } in if (present(value2)) {
      ~ Util\$crash()
    }
  }
}
END


echo "All $count tests passed" 1>&2
