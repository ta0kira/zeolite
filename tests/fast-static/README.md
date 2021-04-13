# Static Linking and `--fast` Mode

Compiling and executing the program in this module should always succeed.
Specifically, static linking in `--fast` mode should not have linker errors.

To compile and run:

```shell
ZEOLITE_PATH=$(zeolite --get-path)
zeolite -p "$ZEOLITE_PATH/tests/fast-static" -I lib/util --fast Program program.0rx
$ZEOLITE_PATH/tests/fast-static/Program
```

The program output should look like this:

```text
Static linking works!
```
