# Zeolite Parser Example

*Also see
[a highlighted version of the example code](https://ta0kira.github.io/zeolite/example/parser).*

This example demonstrates parsing text using a parser-combinator approach
inspired by [`parsec`][parsec]. (The Haskell library that the `zeolite` compiler
originally used to parse source and config files.)

## Notes

This example highlights several distinguishing features of the
[Zeolite language][zeolite]:

- **Encapsulation.** Data encapsulation is mandatory in Zeolite. This forces the
  code author to think about usage patterns before data representation.

- **Factory Functions.** There is no "default construction" in Zeolite; the code
  author must explicitly expose factory functions if needed. In this example,
  most of the factory functions return an interface, rather than the actual
  `concrete` type being constructed.

- **Internal Inheritance.** `ParseState` inherits `ParseContext` internally
  (see `parser.0rp` and `parser.0rx`), which allows `ParseState` to
  conditionally expose the `ParseContext` interface to other functions.

- **Parameter Variance.** Several categories used in this example (e.g.,
  `ParseState` and `ParseContext`) have a *covariant* parameter, which allows
  their parameters to be converted to other types. Specifically:

  - Any `ParseContext<#x>` can be converted to `ParseContext<any>` when calling
    `Parser.run`. (In C++ this would require a `template` and in Java it would
    require using `<?>`.)

  - The `ParseState<all>` returned from `ParseContext.setBrokenInput` can be
    converted to all other `ParseState<#x>`. (In C++ or Java this would require
    the caller to explicitly pass a type parameter to `setBrokenInput`.)

- **Multiple Returns.** Many of the functions in this example return more than
  one value to the caller. (In C++ and Java this would require a data structure
  for grouping objects together.)

- **Test Sources.** The `parser-test.0rt` source file contains a `testcase` that
  the compiler itself can execute.

- **Expression Pragmas.** `parser-test.0rt` uses the `$ExprLookup[MODULE_PATH]$`
  pragma to get the absolute path to its own module in order to locate a data
  file. This might otherwise require hard-coding a path or relying on the tests
  being executed from the module path.

- **Module Config.** The `.zeolite-module` file contains the build configuration
  for this example. This means that building and running the example does not
  require special instructions, scripting, or `Makefile`s.

- **Type Inference.** Several of the function definitions in the `.0rx` files
  request parameter type-inference using `?`, e.g., `ErrorOr:value<?>(match)`.
  This is useful when the parameter value can be easily guessed by the reader of
  the code from its context.

- **The `#self` Param.** Using `#self` as a return type in an `interface` allows
  it to require that an implementation returns *its own type*, rather than
  *the type of the `interface`*. `ParseContext.advance` uses this to allow
  callers of `advance` to get an instance of the derived type rather than an
  instance of `ParseContext`. This is useful because `ParseContext` lacks other
  functions that the derived type defines.

## Running

(Included with compiler package starting with version `0.6.0.0`.)

To run the example:

```shell
# This is just to locate the example code. Not for normal use!
ZEOLITE_PATH=$(zeolite --get-path)

# Compile the example.
zeolite -p "$ZEOLITE_PATH" -r example/parser

# Run the unit tests.
zeolite -p "$ZEOLITE_PATH" -t example/parser
```

(There is currently no binary target in this example.)

[parsec]: https://hackage.haskell.org/package/parsec
[zeolite]: https://github.com/ta0kira/zeolite
