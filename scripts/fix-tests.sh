#!/bin/bash

SAFE='%%'
NOT_NEWLINE='[^%]+'

encode() {
  sed "s/$/$SAFE/g" "$@" | tr -d $'\n'
}

decode() {
  sed "s/$SAFE/\n/g" "$@"
}

BEFORE=$(
encode <<END
testcase ([^{}]+) \{
  (success|crash) ($NOT_NEWLINE)
([^{}]*)\}
END
)

AFTER=$(
encode <<END
testcase \1 \{
  \2
\4}

unittest test {
  \\\\ \3
}
END
)

for f in "$@"; do
  echo "Fixing $f..." 1>&2
  backup="$f~"
  mv -v "$f" "$backup"
  encode "$backup" | sed -r "s@$BEFORE@$AFTER@g" | decode > "$f"
done
