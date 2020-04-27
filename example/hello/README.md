# Zeolite Hello Example

To run the example:

```shell
# This is just to locate the example code. Not for normal use!
ZEOLITE_PATH=$(zeolite --get-path)

# Compile the example.
zeolite -i lib/util -p "$ZEOLITE_PATH" -m HelloDemo example/hello

# Execute the compiled binary.
$ZEOLITE_PATH/example/hello/HelloDemo
```
