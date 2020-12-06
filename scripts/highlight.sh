#!/bin/bash

set -u -e

cd "$(dirname "$0")/.."

source_syntax="$PWD/highlighting/kate/zeolite.xml"
config_syntax="$PWD/highlighting/kate/zeolite-module.xml"

files=($(git ls-files 'example' | egrep '(/\.zeolite-module|.+\.0r[pxt])$'))

output_file() {
  local file=$1
  case "$file" in
    */.zeolite-module) echo "$file" | sed -r "s@(example/.+)/\.zeolite-module@docs/\1/zeolite-module.html@";;
    *)                 echo "$file" | sed -r "s@example/.+@docs/&.html@";;
  esac
}

highlight() {
  local input=$1
  local output=$2
  local syntax
  if [[ "$input" = "$output" ]]; then
    echo "Refusing to overwrite $input with itself" 1>&2
    return 1
  fi
  case "$input" in
    */*.0r?) syntax="Zeolite";;
    */.zeolite-module) syntax="Zeolite Module";;
    *) echo "Unknown file format: $input" 1>&2; return 1;;
  esac
  local command=(skylighting -f html -s "$syntax" -d "$source_syntax" -d "$config_syntax" "$input")
  echo "--- Highlighting $f -> $output with: ${command[@]}" 1>&2
  "${command[@]}" > "$output"
}

for f in "${files[@]}"; do
  output="$(output_file "$f")"
  highlight "$f" "$output"
done
