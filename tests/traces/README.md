# Testing of Possible Traces

This module does not have an executable code. Instead, it is just meant for
sanity-checking of `zeolite`'s collection of possible trace contexts.

To compile:

```shell
ZEOLITE_PATH=$(zeolite --get-path)
zeolite -p $ZEOLITE_PATH -r tests/traces
```

Note that there *will* be warnings about unreachable code.

You can then examine `zeolite --show-traces $ZEOLITE_PATH/tests/traces` to see
what trace contexts `zeolite` picked up during compilation. Every line that is
commented with `TRACED` should have an entry in `--show-traces`.
