# Zeolite Code Highlighter

This example is a work in progress and doesn't do anything yet. Eventually it
will generate syntax-highlighted HTML from Zeolite source files.

The module should still build and the tests should pass, however:

```shell
# This is just to locate the example code. Not for normal use!
ZEOLITE_PATH=$(zeolite --get-path)

# Compile the example.
zeolite -p "$ZEOLITE_PATH" -r example/highlighter

# Run the unit tests.
zeolite -p "$ZEOLITE_PATH" -t example/highlighter
```
