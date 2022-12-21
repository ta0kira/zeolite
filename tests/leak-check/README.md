# Leak and Race Condition Testing

The program in this module is meant to overload the reference-counting system
by forcing memory leaks and race conditions. Ideally, those things *will not*
happen.

To compile the test program:

```shell
ZEOLITE_PATH=$(zeolite --get-path)
zeolite -p $ZEOLITE_PATH -r tests/leak-check
```

---

To check for leaks (requires [`valgrind`](https://valgrind.org/)):

```shell
# The "leak" argument is important.
valgrind --leak-check=yes $ZEOLITE_PATH/tests/leak-check/LeakCheck leak
```

You should get output that looks something like this:

```text
==8487== Memcheck, a memory error detector
==8487== Copyright (C) 2002-2017, and GNU GPL'd, by Julian Seward et al.
==8487== Using Valgrind-3.13.0 and LibVEX; rerun with -h for copyright info
==8487== Command: tests/leak-check/LeakCheck leak
==8487==
==8487==
==8487== HEAP SUMMARY:
==8487==     in use at exit: 2,808 bytes in 73 blocks
==8487==   total heap usage: 2,488 allocs, 2,415 frees, 201,530 bytes allocated
==8487==
==8487== LEAK SUMMARY:
==8487==    definitely lost: 0 bytes in 0 blocks
==8487==    indirectly lost: 0 bytes in 0 blocks
==8487==      possibly lost: 0 bytes in 0 blocks
==8487==    still reachable: 2,808 bytes in 73 blocks
==8487==         suppressed: 0 bytes in 0 blocks
==8487== Reachable blocks (those to which a pointer was found) are not shown.
==8487== To see them, rerun with: --leak-check=full --show-leak-kinds=all
==8487==
==8487== For counts of detected and suppressed errors, rerun with: -v
==8487== ERROR SUMMARY: 0 errors from 0 contexts (suppressed: 0 from 0)
```

*All of the `lost:` fields should be 0.* Note that `still reachable:` is normal
here; Zeolite does some static caching of data to improve performance.

(If you happen to kill the process before it finishes then you will likely see
a lot of leaked memory, which is also normal.)

---

To check for race conditions:

```shell
# The "race" argument is important.
$ZEOLITE_PATH/tests/leak-check/LeakCheck race
```

*Do not use `valgrind` to run in `race` mode!* The latency introduced by
tracking memory usage will eliminate the race conditions that this mode is
intended to introduce, defeating the purpose of the test.

You should see `no race conditions this time` upon success. Any sort of error
message means a crash, and thus a test failure.

---

A more comprehensive test involves leaving `LeakCheck` running indefinitely.

```shell
$ZEOLITE_PATH/tests/leak-check/LeakCheck forever
```

This will cause `LeakCheck` to run for a very long time, while attempting to
leak a few MB of memory each iteration. Since this is a combination of the other
two tests, there isn't any point running this if either of the other two fail.

If the memory size of the running `LeakCheck` process increases over time and
doesn't go back down, there is a memory leak induced by a race condition. The
memory size will vary up and down for the first few minutes, but after 5-10
minutes it should be obvious if the size is increasing.
