# `$TestsOnly$` Visibility Test - Conflicting Definition and Declaration

Compiling this module should **always fail**. It tests that a category
*declared* in a non-`$TestsOnly$` `.0rp` cannot be *defined* in a `$TestsOnly$`
`.0rx`.

To compile:

```shell
ZEOLITE_PATH=$(zeolite --get-path)
zeolite -p $ZEOLITE_PATH -r tests/tests-only3
```

The compiler errors should look something like this:

```text
Zeolite execution failed.
  In compilation of module "/home/ta0kira/checkouts/zeolite/tests/tests-only3"
    Category NotTestsOnly ["tests/tests-only3/public.0rx" (line 21, column 1)] was not declared as $TestsOnly$ ["tests/tests-only3/public.0rp" (line 19, column 1)]
```
