# Zeolite Primes Example

*Also see
[a highlighted version of the example code](https://ta0kira.github.io/zeolite/example/primes).*

This example demonstrates synchronizing two threads using `lib/thread`.

## Notes

There are two threads in this program:

1. The main thread, which deals with user input.
2. The computation thread, which is controlled by the main thread.

When the program starts, the computation thread is stopped, and the main thread
is waiting for user input. Follow the prompts to start, stop, and cancel the
computation thread. The program will print the prime numbers computed during the
time that the computation thread was running.

A few specific points about the example:

- See `flag.0rx` for an example of how `ConditionWait` and `ConditionResume`
  (from `lib/thread`) can be used to start and stop a thread process. (Also
  notice that it uses the type intersection `[ConditionWait&ConditionResume]`.)

- `prime-tracker.0rx` uses both the `$ReadOnly[...]$` and `$Hidden[...]$`
  pragmas to cut down on possible errors when multiple variables of the same
  type are in scope.

- Other than the condition logic in `ThreadFlag`, the other categories in this
  example *are not* thread-safe. On the other hand, the main thread does not
  read or modify any shared state other than the `ThreadFlag` while the
  computation thread is running. Other approaches might require using `Mutex`es
  in other places.

## Running

To run the example:

```shell
# This is just to locate the example code. Not for normal use!
ZEOLITE_PATH=$(zeolite --get-path)

# Compile the example.
zeolite -p "$ZEOLITE_PATH" -r example/primes

# Execute the compiled binary.
$ZEOLITE_PATH/example/primes/PrimesDemo
```
