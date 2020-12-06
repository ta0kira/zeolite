# Bad Path in `.zeolite-module`

Compiling this module should **always fail**. It tests that a mismatch between
`root`/`path` from `.zeolite-module` and the path to the same `.zeolite-module`
causes an error.

To compile:

```shell
ZEOLITE_PATH=$(zeolite --get-path)
zeolite -p $ZEOLITE_PATH -r tests/bad-path
```

The compiler errors should look something like this:

```text
Zeolite execution failed:
  Expected module path from /home/user/zeolite/tests/bad-path/.zeolite-module to match .zeolite-module location but got /dev/null (resolved from root: "" and path: "/dev/null")
```
