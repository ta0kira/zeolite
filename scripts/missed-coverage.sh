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
  echo "Executing:" $(printf ' %q' "$@")
  "$@" 2>&1
}

do_zeolite() {
  execute "${ZEOLITE[@]}" "$@"
}

coverage=$(mktemp)
lines=$(mktemp)
trap 'rm -f "$coverage" "$lines"' EXIT

do_zeolite -r "$path"
do_zeolite -t "$path" --log-traces "$coverage"

cut -d, -f4,4 "$coverage" | fgrep '.0rx' | tr -d '"' | sort -u > "$lines"
fgrep -v "$path/.zeolite-cache/traced-lines" -f "$lines" | sort -g -k2,2 | sort -s -k6,6
