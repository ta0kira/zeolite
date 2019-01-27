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

compile() {
  local temp=$1
  local code=$(cat)
  (
    set -e
    cd "$temp" || exit 1
    { "$compiler" /dev/stdin |& tee -a "$temp/$errors"; } < <(echo "$code$test_base")
    [[ "${PIPESTATUS[0]}" = 0 ]] || return 1
    clang++ -std=c++11 -o "$temp/compiled" \
      -I"$root/capture-thread/include" \
      -I"$root/base" \
      -I"$temp" \
      "$root/capture-thread/src"/*.cc \
      "$root/base"/*cpp \
      "$temp"/*cpp \
      "$main" |& tee -a "$temp/$errors"
    [[ "${PIPESTATUS[0]}" = 0 ]] || return 1
  )
}

expect_error() {
  local name=$1
  shift
  local patterns=("$@")
  local temp=$(mktemp -d)
  local code=$(cat)
  (
    set -e
    cd "$temp" || exit 1
    if "$compiler" /dev/stdin &> "$temp/$errors" < <(echo "$code$test_base"); then
      echo "Test \"$name\": Expected compile error; see output in $temp" 1>&2
      return 1
    fi
  )
  for p in "${patterns[@]}"; do
    if ! egrep -q "$p" "$temp/$errors"; then
      echo "Test \"$name\": Expected pattern '$p' in error; see output in $temp" 1>&2
      return 1
    fi
  done
  echo "Test \"$name\" passed (see output in $temp)" 1>&2
}

expect_runs() {
  local name=$1
  shift
  local patterns=("$@")
  local temp=$(mktemp -d)
  if ! compile "$temp"; then
    echo "Test \"$name\": Expected compilation; see output in $temp" 1>&2
    return 1
  fi
  "$temp/compiled" |& tee -a "$temp/$errors"
  if [[ "${PIPESTATUS[0]}" != 0 ]]; then
    echo "Test \"$name\": Expected execution; see output in $temp" 1>&2
    return 1
  fi
  for p in "${patterns[@]}"; do
    if ! egrep -q "$p" "$temp/$errors"; then
      echo "Test \"$name\": Expected pattern '$p' in error; see output in $temp" 1>&2
      return 1
    fi
  done
  echo "Test \"$name\" passed (see output in $temp)" 1>&2
}

expect_crashes() {
  local name=$1
  local temp=$(mktemp -d)
  if ! compile "$temp"; then
    echo "Test \"$name\": Expected compilation; see output in $temp" 1>&2
    return 1
  fi
  if "$temp/compiled" &> "$temp/$errors"; then
    echo "Test \"$name\": Expected crash; see output in $temp" 1>&2
    return 1
  fi
  echo "Test \"$name\" passed (see output in $temp)" 1>&2
}

expect_runs 'do nothing' <<END
define Test {
  run () {}
}
END

expect_crashes 'require empty' 'require.+empty' 'line 3' <<END
define Test {
  run () {
    ~ Util\$crash()
  }
}
END

expect_error 'self in @type init' 'self' 'line 2' <<END
define Test {
  @type Test value <- self

  run () {}
}
END

expect_error 'self in @category init' 'self' 'line 2' <<END
define Test {
  @category Test value <- self

  run () {}
}
END

expect_error 'self in @type function' 'self' 'line 4' <<END
define Test {
  @type call () -> ()
  call () {
    Test value <- self
  }

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

expect_runs '@type to @category' <<END
concrete Value {}

define Test {
  @category optional Value value <- empty
  @type optional Value value2 <- value

  run () {}
}
END

expect_error '@category to @type' 'value2' 'line 4' <<END
concrete Value {}

define Test {
  @category optional Value value <- value2
  @type optional Value value2 <- empty

  run () {}
}
END

expect_runs 'init in @type to @category' <<END
define Test {
  @category Bool value <- true
  @type Bool value2 <- value

  run () {
    if (value2) {
    } else {
      ~ Util\$crash() // force a crash if false
    }
  }
}
END

expect_crashes 'cycle in init' 'null' 'line 2|line 3' <<END
define Test {
  @type Bool value <- value2
  @type Bool value2 <- value

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

expect_error 'assign while' 'value' 'line 7' <<END
@value interface Value {}

define Test {
  @value process () -> (optional Value)
  process () (value) {
    while (false) {}
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

expect_runs 'present required' <<END
define Test {
  @type create () -> (Test)
  create () {
    return Test{}
  }

  run () {
    Test value <- create()
    if (present(value)) {
    } else {
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
