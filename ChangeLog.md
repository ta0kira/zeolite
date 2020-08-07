# Revision history for zeolite-lang

## 0.8.0.1  -- ????-??-??

### Compiler CLI

* **[fix]** Fixes regression where calling `zeolite` with `-p` would overwrite
  an existing `.zeolite-module`.

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
