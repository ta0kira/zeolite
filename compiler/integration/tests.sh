#!/bin/bash

set -u -e

cd "$(dirname "$0")"

root=$PWD/../..
errors='errors.txt'
main="$PWD/main.cpp"
compiler="$root/compiler/CompilerCxx/compiler"

ghc -i"$root/compiler" "$compiler.hs"

compile() {
  local temp=$1
  local code=$(cat)
  (
    set -e
    cd "$temp" || exit 1
    { "$compiler" /dev/stdin |& tee "$temp/$errors"; } < <(echo "$code")
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
  local temp=$(mktemp -d)
  local code=$(cat)
  (
    set -e
    cd "$temp" || exit 1
    if "$compiler" /dev/stdin &> "$temp/$errors" < <(echo "$code"); then
      echo "Test \"$name\": Expected compile error; see output in $temp"
    fi
  )
}

expect_runs() {
  local name=$1
  local temp=$(mktemp -d)
  if ! compile "$temp"; then
    echo "Test \"$name\": Expected compilation; see output in $temp" 1>&2
    return 1
  fi
  "$temp/compiled" |& tee -a "$temp/$errors"
  if [[ "${PIPESTATUS[0]}" != 0 ]]; then
    echo "Test \"$name\": Expected execution; see output in $temp" 1>&2
  fi
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
  fi
}

expect_runs 'do nothing' <<END
concrete Test {
  @category run () -> ()
}

define Test {
  run () {}
}
END

expect_crashes 'require empty' <<END
concrete Test {
  @category run () -> ()
}

define Test {
  run () {
    ~ require(empty)
  }
}
END

expect_error 'self in @category' <<END
concrete Test {
  @category run () -> ()
}

define Test {
  run () {
    ~ self
  }
}
END
