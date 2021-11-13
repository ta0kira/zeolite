# Testing of Possible Traces

This module does not have an executable code. Instead, it is just meant for
sanity-checking of `zeolite`'s collection of possible trace contexts.

To compile:

```shell
ZEOLITE_PATH=$(zeolite --get-path)
zeolite -p $ZEOLITE_PATH -r tests/traces
```

Note that there *will* be warnings about unreachable code.

You can then examine `$ZEOLITE_PATH/tests/traces/.zeolite-cache/traced-lines` to
see what trace contexts `zeolite` picked up during compilation. Every line
that is commented with `TRACED` should have an entry in `traced-lines`.

`traced-lines` can be used with `zeolite -t --log-traces traces.csv ...` to
determine which lines of code have not been covered by the executed tests.
