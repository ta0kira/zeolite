# Public Dependency Warning

Compiling this module should succeed with a warning about a public dependency
that does not need to be public.

To compile:

```shell
ZEOLITE_PATH=$(zeolite --get-path)
zeolite -p $ZEOLITE_PATH -r tests/warn-public -f
```

The compiler output should look something like this:

```text
Warning: Dependency "internal" does not need to be public
Zeolite execution succeeded.
```
