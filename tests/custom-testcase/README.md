# Custom `Testcase` Compiler Failures.

_All_ tests this module should **always fail**. It tests that compiling a
`testcase` with a custom `Testcase` catches type issues.

To compile and run the tests:

```shell
ZEOLITE_PATH=$(zeolite --get-path)
zeolite -p $ZEOLITE_PATH -r tests/custom-testcase
zeolite -p $ZEOLITE_PATH -t tests/custom-testcase
```

_Every_ test should fail with an error related to `custom testcase`.
