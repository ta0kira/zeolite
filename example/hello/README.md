# Zeolite Hello Example

*Also see
[a highlighted version of the example code](https://ta0kira.github.io/zeolite/example/hello).*

To run the example:

```shell
# This is just to locate the example code. Not for normal use!
ZEOLITE_PATH=$(zeolite --get-path)

# Compile the example.
zeolite -p "$ZEOLITE_PATH/example/hello" -I lib/util --fast HelloDemo hello-demo.0rx

# Execute the compiled binary.
$ZEOLITE_PATH/example/hello/HelloDemo
```
