# Revision history for zeolite-lang

## 0.11.1.0  -- ????-??-??

### Language

  * **[fix]** Fixes a bug where a named return with a primitive type was not
    getting set in an explicit `return` statement. This only affected code that
    used primitive named returns inside of a `cleanup` block where the
    corresponding `in` block used an explicit `return` statement.

    ```text
    @type call () -> (Int)
    call () (x) {
      cleanup {
        \ foo(x)     // x was not getting set for this call
      } in return 1  // 1 was still returned from the function as expected
    }
    ```

    This *did not* affect non-primitive types such as `String` and user-defined
    `concrete` and `interface` categories.

### Libraries

  * **[new]** Adds `Math.abs` for `Int` absolute value to `lib/math`.

## 0.11.0.0  -- 2020-12-09

### Compiler CLI

* **[breaking]** Switches from [`parsec`][parsec] to [`megaparsec`][megaparsec]
  for parsing, to allow for better structuring of compile-time error messages.
  This should not affect syntax acceptance, but it has additional transitive
  dependencies that might prevent installation of `zeolite`.

### Language

* **[breaking]** Several `cleanup` fixes:

  * **[fix]** Fixes a bug in `cleanup` that caused the `scoped` block to be
    prepended to it when it was inlined in the `in` block.

  * **[fix]** Fixes a bug where `cleanup` might use an uninitialized named
    return when inlined at a `break` or `continue` statement in a `while` loop.
    Such situations will now cause a compilation error. Previously, this could
    have allowed an uninitialized value to be passed around, which would only be
    noticed with a `Function called on null value` failure at some point.

  * **[fix]** Fixes inlining of `cleanup` blocks when `continue` is used inside
    of the `in` block. Previously, `cleanup` was skipped when `continue` was
    used inside of the respective `in` block.

  * **[breaking]** Disallows explicit `break` and `continue` inside of `cleanup`
    blocks. Previously this was allowed (but `return` wasn't), which led to
    ambiguity when the `in` block contained a `return`.

  * **[new]** Allows `cleanup` to refer to variables created at the top level of
    the `in` block.

    ```text
    cleanup {
      \ foo(bar)                    // bar is created below
    } in Type bar <- Type.create()  // cleanup is inlined after this
    ```

## 0.10.0.0  -- 2020-12-05

### Language

* **[breaking]** Major changes to `.0rt` syntax:

  * **[breaking]** Removes the expression argument from `success` and `crash`
    `testcase` types. Use one or more `unittest` (see next point) instead.

  * **[new]** Adds `unittest` keyword to allow the user to define multiple
    tests within a single `success` `testcase`.

    ```text
    testcase "simple tests" {
      success
    }

    unittest checkInvariant1 {
      // this is a regular procedure with no return
    }

    unittest checkInvariant2 {
      // this is a regular procedure with no return
    }
    ```

    This allows multiple tests to use the same setup, while still being able to
    track multiple test failures. `error` and `crash` `testcase` types are still
    available.

  * **[breaking]** Allows `testcase` to statically set `Argv` (see `lib/util`)
    using the `args` keyword. By default, only the program name is set.
    Previously, `Argv` used whatever was passed to the test binary.

    ```text
    testcase "with static Argv" {
      success
      args "arg1" "arg2"
    }

    unittest test {
      // something that requires Argv.global()
    }
    ```

    Note that this *isn't* a
    library change; this also affects C++ extensions that use `Argv` from
    `base/logging.hpp`.

  * **[new]** Adds `compiles` mode for `testcase`. This is like `success` except
    nothing is executed. (`success` requires at least one `unittest`, whereas
    `compiles` will not execute anything even if `unittest` are present.)

### Libraries

* **[new]** Adds the `lib/testing` library with basic helpers for unit tests.

### Compiler CLI

* **[new]** Updates parsing of `.zeolite-module` to allow any ordering of the
  fields. Previously, the fields needed to be in a very specific order.

* **[fix]** Adds compile-time protection against self-referential expression
  macros defined in `expression_map:` in `.zeolite-module`. Previously, the
  result was infinite recursion that could exhaust system resources.

* **[breaking]** Adds an error when `root`/`path` in `.zeolite-module` differs
  from the location of the respective `.zeolite-module`. This is to catch both
  typos (e.g., copy-and-paste errors) and ambiguities.

## 0.9.0.0  -- 2020-11-23

### Compiler CLI

* **[breaking]** Major changes to C++ extensions:

  * **[breaking]** Corrects hole in `$ModuleOnly$` visibility in C++ extensions.
    Previously, C++ extensions could directly access categories defined in
    `$ModuleOnly$` sources in dependencies.

  * **[breaking]** Randomizes cache paths and `namespace`s for generated C++
    code so that C++ extensions cannot use static `#include` paths to circumvent
    the visibility rules of a dependency.

  * **[breaking]** Makes `TypeInstance`s shared in generated C++ and in C++
    extensions, to allow for better memory management of `@type`s in the future.

  * **[breaking]** Adds guards to C++ headers for `$TestsOnly$` categories, to
    prevent them from being used in non-`$TestsOnly$` C++ extensions.

* **[fix]** Fixes regression where calling `zeolite` with `-p` would overwrite
  an existing `.zeolite-module`.

### Language

* **[breaking]** Syntax changes:

  * **[breaking]** Changes the syntax for calling `@category` functions from
    `$$` to `:`, e.g., `Foo:create()` instead of `Foo$$create()`. A new error
    message (to be removed later) will remind the user to stop using `$$`.

  * **[breaking]** Changes the syntax for calling `@type` functions from `$`
    to `.`, e.g., `Foo.create()` instead of `Foo$create()`. A new error message
    (to be removed later) will remind the user to stop using `$`.

* **[breaking]** Changes to param usage:

  * **[breaking]** Updates param inference to be more accurate. The new
    algorithm will infer a param type iff the best choice is unambiguous given
    the expected and actual function arguments.

  * **[breaking]** Disallows bounding a single param both above and below, e.g.,
    having both `#x requires Foo` and `#x allows Bar`. This is primarily to
    prevent cycles and contradictions in the type system.

    Previously, the *best case* for such restrictions was that there are
    extremely few choices for `#x`, and the *worst case* was that it implies
    `Bar` converts to `Foo` when it isn't true, thus making `#x` unusable.
    Having either of those requirements likely indicates a design flaw.

* **[breaking]** Prohibits having a `$TestsOnly$` definition for a
  non-`$TestsOnly$` category. Previously, a `concrete` category declared in a
  non-`$TestsOnly$` `.0rp` could be `define`d in a `$TestsOnly$` `.0rx`, which
  creates a loophole that allows binaries to utilize `$TestsOnly$` code.

* **[fix]** Fixes a latent type-checking bug that could occur when attempting to
  assign a value with an intersection type to a variable with a union type, if
  one or both types has another nested type, e.g., `[A&B]`→`[[A&B]|C]` or
  `[[A|B]&C]`→`[A|B]`. This change *shouldn't* cause new type-checking failures.

### Libraries

* **[fix]** Implements `subSequence` for `Argv` in `lib/util`. This is required
  by `ReadPosition` but was forgotten. Calling it prior to this would cause a
  failure.

## 0.8.0.0  -- 2020-08-07

### Language

* **[breaking]** Makes the semantics of `cleanup` more consistent:

  * **[breaking]** Disallows statements that modify `return` values within
    `cleanup` blocks, i.e., `return` with values and assigning named returns.

  * **[new]** Allows `cleanup` to access named returns that are initialized
    within the corresponding `in` statement. Previously, access required
    initialization *before* the `in` statement.

  * **[new]** An explicit *positional* return (e.g., `return 1, 2`) will assign
    the values to the respective named returns, if applicable. This will make
    the actual return values available within `cleanup`.

    ```text
    @type get () -> (Int)
    get () (foo) {   // foo is the name of the return variable
      cleanup {
        \ bar(foo)   // foo has been initialized by the time this is called
      } in return 1  // 1 is assigned to foo here
    }
    ```

  * **[fix]** Adds unwinding of `cleanup` blocks when used with `break` and
  `continue`, respecting loop boundaries. Previously, `cleanup` was ignored.

* **[breaking]** Skips compilation of unreachable statements. The target
  use-case is temporary changes to code that circumvent parts of a procedure,
  e.g., an early `return` for the purposes of debugging.

* **[breaking]** Marks statements following `break` and `continue` as
  unreachable.

### Compiler CLI

* **[behavior]** Adds a compiler warning for `public_deps` that are not required
  by public `.0rp` sources within the same module, since they are potentially
  just cluttering the public namespace of the module.

## 0.7.1.0  -- 2020-07-13

### Language

* **[new]** Adds the `$SourceContext$` macro, which inserts a `String` with
  info about source file and code location.

* **[fix]** Adds context to error messages related to inherited functions that
  have not been overridden.

## 0.7.0.2  -- 2020-05-21

### Language

* **[fix]** Improves the likelihood of successful type inference when unions,
  intersections, or filters are combined with inheritance and type nesting.

## 0.7.0.1  -- 2020-05-20

### Language

* **[fix]** Fixes an edge-case where type-inference can be influenced by a
  param filter for a param with the same name in the scope that is calling the
  function. (For example, having `#x requires Foo` in scope while calling a
  function that also happens to have a param named `#x`.)

* **[fix]** Fixes an edge-case where parameter substitution at the type level
  can clash with a type parameter scoped to a function. (Related to the above,
  but actually a separate issue discovered while solving the former.)

* **[behavior]** Shows inferred-type assignments in error messages related to
  expressions containing inferred types.

## 0.7.0.0  -- 2020-05-19

### Language

* **[new]** Adds limited inference of type parameters in function calls. The new
  syntax is `call<?,Int>(foo,bar)`, where `?` designates that inference is
  requested for the first position.

* **[behavior]** Reduces the memory cost of `$TraceCreation$` by avoiding
  storing the entire trace text.

### Compiler CLI

* **[behavior]** Improves handling of I/O errors when calling `zeolite`, and
  allows compilation to continue in `-r`/`-R` modes if compilation of one module
  fails.

## 0.6.0.0  -- 2020-05-14

### Compiler CLI

* **[behavior]** Improves error messages for type mismatches and return-count
  mismatches.

* **[behavior]** Improves the efficiency of loading metadata for dependencies.

* **[breaking]** Fixes module-staleness check when running tests (`-t`).

* **[breaking]** Prevents binaries from being created from main categories that
  are defined in `$TestsOnly$` sources. (This was the original intention, but it
  was missed the first time around.)

### Language

* **[new]** Allows `@category` members in `concrete` categories to refer to each
  other during initialization.

* **[new]** Adds the `$ExprLookup[`*`MACRO_NAME`*`]$` pragma, which allows the
  user to define expression substitutions in `.zeolite-module`.

* **[new]** Adds the `$NoTrace$` pragma, which skips generating stack-trace
  information for specific functions. This is useful for deeply-recursive
  functions whose full trace would not be useful.

* **[new]** Adds the `$TraceCreation$` pragma, which appends a trace for the
  creation of a value when there is a crash in one of its functions. This is
  useful when the crash stems from value initialization.

* **[fix]** Fixes parsing of the `'"'` `Char`-literal.

### Libraries

* **[new]** Adds the `ErrorOr<#x>` category to `lib/util`. This allows functions
  to return either the expected type or an error message.

* **[new]** Adds the `Void` category to `lib/util`. This can be used to ignore
  type parameters when a value still needs to be instantiated.

* **[new]** Adds the `readAll` function to `TextReader` (in `lib/util`) to
  support reading an entire file at once.

* **[breaking]** Adds crashes when attempting to read `RawFileReader` or write
  `RawFileWriter` if there is a preexisting file error.

## 0.5.0.0  -- 2020-05-12

* **[new]** Adds compiler support for pragmas, which will allow compiler
  features that aren't a part of the language:

  - **[new]** `$ModuleOnly$` pragma for `.0rp` files, which limits their
    visibility to the modules that own them.

  - **[new]** `$TestsOnly$` pragma for `.0rp` files, which limits their
    visibility to `.0rt` files, and `.0rx` files that also use `$TestsOnly$`.

* **[new]** Adds support for global seach paths for dependencies via a
  plain-text file at `$(zeolite --get-path)/global-paths`. This can be used to
  set up a separate directory for general-use Zeolite modules.

* **[breaking]** Better handling of symbols from dependencies. This might break
  code that inavertently relied on broken visibility. In particular, `..` in a
  dependency path precludes it from resolving to a system module.

## 0.4.1.0  -- 2020-05-05

* **[new]** Adds a compiler mode (`--show-deps`) to display the symbolic
  dependencies of a module.

* **[new]** Adds a compiler mode (`--fast`) to quickly compile a binary from a
  single source file without needing to create a module.

* **[new]** Adds a compiler mode (`-R`) to recursively recompile modules.

* **[new]** Improves thread-safety of internal code so that thread support can
  be safely added later. (Probably via a library.)

* **[new]** Adds the `Builder` interface to make `String` concatenation more
  efficient.

* **[new]** Adds support for basic mathematical `Float` operations, such as
  `sin`, `exp`, and `sqrt`.

* **[new]** Adds support for bitwise `Int` operations.

* **[fix]** Fixes broken `--templates` mode when builtin types are used.

## 0.4.0.0  -- 2020-05-05

* **[new]** Allows modules to specify custom linker flags and private include
  paths in `.zeolite-module`. This lets categories written in C++ depend on
  external libraries written in other languages.

* **[behavior]** Adds optimization of dependency inclusion for categories that
  are defined in C++ sources. This should eliminate linking in object files that
  are not needed by the binary.

* **[behavior]** Adds checks to prevent a module from defining a category that
  was declared in another module.

* **[breaking]** Updates the `.zeolite-module` format to require associating
  externally-defined categories with the source files that define them. This
  allows finer-grained linking of binaries and tests.

## 0.3.0.0  -- 2020-05-03

* **[breaking]** Updates syntax for discarding all returns to use `\` instead of
  `~`. This will allow `~` to be used for bitwise-not later on. (Not to mention
  that `~` was kind of ugly in that context.)

* **[breaking]** Cleans up multiple-`return` syntax by removing `{}`. This
  applies to *both* the `return` and the assignment.

## 0.2.0.0  -- 2020-05-03

* **[breaking]** Requires that `concrete` categories defined in `.cpp` files be
  listed as `external` in `.zeolite-module`. This allows the compiler to ensure
  that all categories have a definition, which helps avoid linker errors.

* **[breaking]** Gives `.zeolite-module` configs a proper file format.

* **[new]** Adds version checking to cached module data.

## 0.1.3.0  -- 2020-05-01

* **[new]** Adds support for more versions of GHC.

## 0.1.2.0  -- 2020-04-28

* **[fix]** Fixes a parser issue with empty `{}` blocks following `scoped`.

* **[behavior]** Updates `cleanup` procedures to allow setting named-return
  values. Previously, initializing a named return in `cleanup` was not
  sufficient.

* **[behavior]** Updates `zeolite-setup` to unconditionally rebuild supporting
  libraries. Incidentally, this causes all existing user modules to be out of
  date.

## 0.1.1.0  -- 2020-04-27

* **[behavior]** Set the default output path for binaries to the module's path
  rather than the current directory.

* **[new]** Allows a base path (`-p`) with recompile mode (`-r`).

## 0.1.0.0  -- 2020-04-27

* First version. Released on an unsuspecting world.

[megaparsec]: https://hackage.haskell.org/package/megaparsec
[parsec]: https://hackage.haskell.org/package/parsec
