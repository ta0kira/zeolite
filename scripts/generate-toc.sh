#!/bin/bash

set -u -e

cd "$(dirname "$0")/.."

readme='README.md'

paste -d '' \
  <(grep '##' "$readme" | sed '1d' |
    sed -r 's/#([a-z][a-zA-Z0-9]*)/@@@\1/g' |  # temporarily rename params
    sed -r 's/## (.+)/- [\1]/' |
    sed 's/#/  /g' |
    sed -r 's/@@@([a-z][a-zA-Z0-9]*)/#\1/g') \
  <(grep '##' "$readme" | sed '1d' |
    sed -r 's/#+ +//' |
    tr -d '[:punct:]' |
    tr '[:upper:]' '[:lower:]' |
    sed 's/ /-/g' |
    sed 's/.*/(#&)/')
