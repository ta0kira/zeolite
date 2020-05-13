# Revision history for zeolite-lang

## 0.6.0.0  -- ????-??-??

* **[new]** Adds the `$ExprLookup[`*`MACRO_NAME`*`]$` pragma, which allows the
  user to define expression substitutions in `.zeolite-module`.

* **[new]** Adds the `$NoTrace$` pragma, which skips generating stack-trace
  information for specific functions. This is useful for deeply-recursive
  functions whose full trace would not be useful.

* **[breaking]** Prevents binaries from being created from main categories that
  are defined in `$TestsOnly$` `.0rx` sources. (This was the original intention,
  but it was missed the first time around.)

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
