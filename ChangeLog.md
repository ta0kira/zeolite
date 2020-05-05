# Revision history for zeolite-lang

## 0.4.0.0  -- ????-??-??

* **[new]** Allows modules to specify custom linker flags. This lets categories
  written in C++ to specify extra libraries to link to, allowing Zeolite
  extensions of libraries written in other languages.

* **[new]** Improves optimization of dependency inclusion for categories that
  are defined in C++ sources.

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
