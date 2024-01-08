# Zeolite Code Highlighter

This example is a fully-functional syntax highlighter for Zeolite source files.
You can see the highlighting of the code in this example
[here](https://ta0kira.github.io/zeolite/example/highligher/).

To build and test the example code:

```shell
# This is just to locate the example code. Not for normal use!
ZEOLITE_PATH=$(zeolite --get-path)

# Compile the example.
zeolite -p "$ZEOLITE_PATH" -r example/highlighter

# Run the unit tests.
zeolite -p "$ZEOLITE_PATH" -t example/highlighter
```

To generate an HTML file for Zeolite source files (after building the example):

```shell
example/highlighter/ZeoliteHighlight "Your Page Title" your-source-file.0rx
```
