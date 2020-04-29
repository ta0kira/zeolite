# Zeolite Regex Example

*Also see
[a highlighted version of the example code](https://ta0kira.github.io/zeolite/example/regex).*

To run the example:

```shell
# This is just to locate the example code. Not for normal use!
ZEOLITE_PATH=$(zeolite --get-path)

# Compile the example.
zeolite -p "$ZEOLITE_PATH" -i lib/util -m RegexDemo example/regex

# Run the unit tests.
zeolite -p "$ZEOLITE_PATH" -t example/regex

# Execute the compiled binary.
$ZEOLITE_PATH/example/regex/RegexDemo
```
