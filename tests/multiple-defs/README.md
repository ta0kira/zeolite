# Multiple Definition Failure

Compiling this module should **always fail**. It tests a compiler check that
ensures that multiple `.0rx` files do not define the same `concrete` category
from a `.0rp` file.

To compile:

```shell
../../zeolite -r .
```

The compiler error should look something like this:

```text
Public category Type is defined 2 times
  Defined at "tests/multiple-defs/private2.0rx" (line 19, column 1)
  Defined at "tests/multiple-defs/private1.0rx" (line 19, column 1)
```
