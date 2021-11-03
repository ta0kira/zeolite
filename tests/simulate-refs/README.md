# Zeolite Reference Simulation

This module was created to investigate a race condition in the implementation
of `weak`. (See [#186](https://github.com/ta0kira/zeolite/issues/186).) It
models the previous behaviors (before the fix) of `BoxedValue` and `WeakValue`
using state machines that are stochastically interleaved, in an attempt to
determine which sequences of events caused the memory leak.

This simulation has 2 modes:

1. Run `SimulateRefs` with no arguments to use the current behavior. You should
   see no errors, and it should continue to run forever.
2. Run with `broken` as the only argument to use the previous behavior that had
   the memory leak. You should see quite a few errors fairly quickly.

Note that this simulation code isn't specific to Zeolite; it could have been
written in any other language. Any errors output by the simulation are
predictions made by the model, rather than actual runtime errors in the program.

To compile and run the simulation:

```shell
ZEOLITE_PATH=$(zeolite --get-path)
zeolite -p $ZEOLITE_PATH -r tests/simulate-refs
$ZEOLITE_PATH/tests/simulate-refs/SimulateRefs
```
