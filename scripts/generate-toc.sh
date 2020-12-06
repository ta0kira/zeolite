#!/bin/bash

set -u -e

cd "$(dirname "$0")/.."

readme='README.md'

paste -d '' \
  <(grep '##' "$readme" | sed '1d' |
    sed -r 's/## (.+)/- [\1]/' |
    sed 's/#/  /g') \
  <(grep '##' "$readme" | sed '1d' |
    sed -r 's/#+ +//' |
    tr -d '[:punct:]' |
    tr '[:upper:]' '[:lower:]' |
    sed 's/ /-/g' |
    sed 's/.*/(#&)/')
