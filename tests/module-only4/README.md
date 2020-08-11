# `$ModuleOnly$` Pragma Test - C++ Extension

This module tests that the `$ModuleOnly$` categories can be defined and accessed
in C++ extensions in the same module.

To compile and run the test:

```shell
ZEOLITE_PATH=$(zeolite --get-path)
zeolite -p $ZEOLITE_PATH -r tests/module-only4
zeolite -p $ZEOLITE_PATH -t tests/module-only4
```
