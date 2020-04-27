# Multiple Definition Failure

Compiling this module should **always fail**. It tests a compiler check that
ensures that multiple `.0rx` files do not define the same `concrete` category
from a `.0rp` file.

To compile:

```shell
ZEOLITE_PATH=$(zeolite --get-path)
zeolite -r $ZEOLITE_PATH/tests/multiple-defs
```

The compiler error should look something like this:

```text
Public category Type ["tests/multiple-defs/public.0rp" (line 19, column 1)] is defined 2 times
  Defined at "tests/multiple-defs/private2.0rx" (line 19, column 1)
  Defined at "tests/multiple-defs/private1.0rx" (line 19, column 1)
```
