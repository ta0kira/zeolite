#!/usr/bin/env bash

if [[ $# -lt 1 ]]; then
  echo "$0 [zeolite files...]" 1>&2
  exit 1
fi

files=("$@")
expressions=(
  # intersections, unions, *not* param variance
  -e '/^concrete|^@.+ interface/!s/([^-< ])([&|])([^> .])/\1 \2 \3/g'
  # various args/returns
  -e '/Copyright/!s/,([^ ])/, \1/g'
  # empty procedures
  -e 's/ \{\}/ { }/g'
)

sed -Ei "${expressions[@]}" "${files[@]}"
