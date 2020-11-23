# Multiple Definition Failure

Compiling this module should **always fail**. It tests a compiler check that
ensures that `concrete` categories have exactly one definition or are declared
in a `category_source` in the `extra_files:` section of `.zeolite-module`.

To compile:

```shell
ZEOLITE_PATH=$(zeolite --get-path)
zeolite -p $ZEOLITE_PATH -r tests/check-defs
```

The compiler errors should look something like this:

```text
Zeolite execution failed.
  Category Type ["tests/check-defs/public.0rp" (line 19, column 1)] is defined 2 times
    Defined at "tests/check-defs/private2.0rx" (line 19, column 1)
    Defined at "tests/check-defs/private1.0rx" (line 19, column 1)
  Category Undefined ["tests/check-defs/public.0rp" (line 21, column 1)] has not been defined or declared external
```
