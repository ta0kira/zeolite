# `$TestsOnly$` Visibility Test - Binaries

Compiling this module should **always fail**. It tests that the `$TestsOnly$`
pragma limits visibility of `.0rx` categories to only test binaries.

To compile:

```shell
ZEOLITE_PATH=$(zeolite --get-path)
zeolite -p $ZEOLITE_PATH -r tests/tests-only2
```

The compiler errors should look something like this:

```text
Compiler errors:
No matches for main category Testing ($TestsOnly$ sources excluded)
```
