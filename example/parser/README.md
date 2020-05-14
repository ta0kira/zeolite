# Zeolite Parser Example

*Also see
[a highlighted version of the example code](https://ta0kira.github.io/zeolite/example/parser).*

This example demonstrates parsing a text file using a parser-combinator
approach. It also uses monad-like semantics to handle errors that occur during
parsing, in the absense of actual monads and exceptions.

The code here is more about testing the limitations of the Zeolite language, and
less about demonstrating proper language usage.

To run the example:

```shell
# This is just to locate the example code. Not for normal use!
ZEOLITE_PATH=$(zeolite --get-path)

# Compile the example.
zeolite -p "$ZEOLITE_PATH" -r example/parser

# Run the unit tests.
zeolite -p "$ZEOLITE_PATH" -t example/parser
```

There is currently no binary target in this example. This example also uses a
`.zeolite-module`, unlike the other examples.
