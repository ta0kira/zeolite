# `$ModuleOnly$` Pragma Test - Visibility in C++

Compiling this module should **always fail**. It tests that the `$ModuleOnly$`
pragma limits visibility even in C++ source files.

To compile:

```shell
ZEOLITE_PATH=$(zeolite --get-path)
zeolite -p $ZEOLITE_PATH -R tests/module-only3
```

The compiler errors should look something like this:

```text
tests/module-only3/source1.cpp:19:10: fatal error: 'Category_Type1.hpp' file not found
#include "Category_Type1.hpp"
         ^~~~~~~~~~~~~~~~~~~~
1 error generated.
Execution of /usr/bin/clang++ failed

```

Importantly, there *should not* be any errors related to `Type2`; only `Type1`.
