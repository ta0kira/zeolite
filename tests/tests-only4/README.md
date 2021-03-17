# `$TestsOnly$` Pragma Test - Enforcement in C++

Compiling this module should **always fail**. It tests that the `$TestsOnly$`
pragma is enforced even in C++ source files.

To compile:

```shell
ZEOLITE_PATH=$(zeolite --get-path)
zeolite -p $ZEOLITE_PATH -R tests/tests-only4
```

The compiler errors should look something like this:

```text
In file included from tests/tests-only4/source2.cpp:20:
Category_Type2.hpp:4:2: error: Category Type2 can only be used by $TestsOnly$ categories
#error Category Type2 can only be used by $TestsOnly$ categories
 ^
1 error generated.
Execution of /usr/bin/clang++ failed
```

Importantly, there *should not* be any errors related to `source1.cpp`; only
`source2.cpp`.
