# Revision history for zeolite-lang

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
