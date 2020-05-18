# `$ModuleOnly$` Pragma Test

Compiling this module should **always fail**. It tests that the `$ModuleOnly$`
pragma limits visibility to only `.0rx` and `.0rt` sources in the same module,
and `.0rp` sources in the same module that also have the `$ModuleOnly$` pragma.

To compile:

```shell
ZEOLITE_PATH=$(zeolite --get-path)
zeolite -p $ZEOLITE_PATH -R tests/module-only
```

The compiler errors should look something like this:

```text
Zeolite execution failed.
  In creation of val1 at "tests/module-only/private.0rx" (line 22, column 3)
    Type Type1 not found
```
