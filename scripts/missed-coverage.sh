#!/bin/bash

set -u -e

if [ $# -lt 2 ]; then
  echo "$0 [module path] [zeolite command...]" 1>&2
  exit 1
fi

path=$1
shift

ZEOLITE=("$@")

execute() {
  echo "Executing:" $(printf ' %q' "$@") 1>&2
  "$@"
}

do_zeolite() {
  execute "${ZEOLITE[@]}" "$@"
}

coverage=$(mktemp)
trap 'rm -f "$coverage"' EXIT

do_zeolite -r "$path"
do_zeolite -t "$path" --log-traces "$coverage"
do_zeolite --missed-lines "$coverage" "$path" | sort -g -k2,2 | sort -s -k6,6
