# `$ModuleOnly$` Pragma Test - Dependency

Compiling this module should **always fail**. It tests that the `$ModuleOnly$`
pragma limits visibility to only `.0rx` and `.0rt` sources in the same module.

To compile:

```shell
ZEOLITE_PATH=$(zeolite --get-path)
zeolite -p $ZEOLITE_PATH -R tests/module-only
```

The compiler errors should look something like this:

```text
Zeolite execution failed.
  In compilation of module "/home/ta0kira/checkouts/zeolite/tests/module-only"
    In creation of val1 at "tests/module-only/private.0rx" (line 22, column 3)
      Type Type1 not found
```
