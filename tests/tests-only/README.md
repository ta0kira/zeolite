# `$TestsOnly$` Visibility Test - Incremental

Compiling this module should **always fail**. It tests that the `$TestsOnly$`
pragma limits visibility to only `.0rx` sources that also have the `$TestsOnly$`
pragma, as well as all `.0rt` sources.

To compile:

```shell
ZEOLITE_PATH=$(zeolite --get-path)
zeolite -p $ZEOLITE_PATH -r tests/tests-only
```

The compiler errors should look something like this:

```text
Compiler errors:
Definition for Type1 ["tests/tests-only/private.0rx" (line 19, column 1)] does not correspond to a visible category in this module
```
