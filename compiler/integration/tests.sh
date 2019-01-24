#!/bin/bash

set -u -e

cd "$(dirname "$0")"

compiler='./compile_test'

ghc -i$PWD/.. "$compiler.hs"

expect_compiles() {
  local patterns=("$@")
  local output=$("$compiler" /dev/stdin)
  # TODO: Does not catch exit error.
  if [[ $? != 0 ]]; then
    echo "Expected compilation but got "
    echo "$output"
    return 1
  else
    for p in "${patterns[@]}"; do
      if ! (echo "$output" | egrep -q "$p"); then
        echo "Expected pattern '$p' in output"
        return 1
      fi
    done
  fi 1>&2
}

expect_compiles <<END
@type interface Create<#x|#y> {
  #y defines Create<#x,#y>
  create (#x) -> (#y)
}

@value interface Get<|#x> {
  get () -> (#x)
}

@value interface Set<#x|> {
  set (#x) -> ()
}
END
