#!/bin/bash

readme='README.md'

paste -d '' \
  <(grep '##' "$readme" |
    sed -r 's/## (.+)/- [\1]/' |
    sed 's/#/  /g') \
  <(grep '##' "$readme" |
    sed -r 's/#+ +//' |
    tr -d '[:punct:]' |
    tr '[:upper:]' '[:lower:]' |
    sed 's/ /-/g' |
    sed 's/.*/(#&)/')
