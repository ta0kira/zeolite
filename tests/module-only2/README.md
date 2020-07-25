# `$ModuleOnly$` Pragma Test - Local

Compiling this module should **always fail**. It tests that the `$ModuleOnly$`
pragma limits visibility to `.0rp` sources in the same module that also have the
`$ModuleOnly$` pragma.

To compile:

```shell
ZEOLITE_PATH=$(zeolite --get-path)
zeolite -p $ZEOLITE_PATH -R tests/module-only2
```

The compiler errors should look something like this:

```text
Zeolite execution failed.
  In compilation of module "/home/ta0kira/checkouts/zeolite/tests/module-only2"
    Type Type1 ["tests/module-only2/public.0rp" (line 20, column 3)] not found
```
