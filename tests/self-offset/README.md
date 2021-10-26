# Testing Pointer Offset of `self`

This module tests that the generated C++ correctly accounts for pointer offsets
when C++ extensions use `VAR_SELF`.

To compile and run the test:

```shell
ZEOLITE_PATH=$(zeolite --get-path)
zeolite -p $ZEOLITE_PATH -r tests/self-offset
zeolite -p $ZEOLITE_PATH -t tests/self-offset
```
