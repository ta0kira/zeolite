# Revision history for zeolite-lang

## 0.20.0.0  -- ????-??-??

### Compiler CLI

* **[breaking]** Removes all passing of `Param_self` in C++ extensions. Use
  `PARAM_SELF` where `Param_self` was previously used, and delete the
  `Param_self` argument from function signatures. This breaks all existing
  extensions that have `@type` functions.

* **[breaking]** Removes `AnonymousOrder` C++ class, previously used for
  creating iterators for C++ extensions.

### Libraries

* **[breaking]** Removes iterators from `lib/util` (`iterator.0rp`) because they
  were awkward, inefficient, and had no current intended use-cases.

### Language

* **[breaking]** Adds the `immutable` category property. This marks all `@value`
  members in the respective `concrete` implementations as read-only. (This is
  breaking because `immutable` is now reserved.)

* **[new]** Updates `String.builder()` to accept `Formatted` values, rather than
  just accepting `String`.

* **[behavior]** Reduces memory size of `Bool`, `Char`, `Float`, `Int`, and
  references to all other value objects.

* **[behavior]** Efficiency improvements for passing values in function calls.

* **[behavior]** Skips calling `get` in `traverse` if the value is discarded.

  ```text
  // The _ suppresses the call to get() on the returned Order<Int> value.
  traverse (Counter.zeroIndexed(100) -> _) {
    // ...
  }
  ```

* **[behavior]** Makes dispatching of function calls quite a bit more efficient.

## 0.19.0.0  -- 2021-10-25

### Compiler CLI

* **[breaking]** Changes how `BoxedValue` is created for new `TypeValue`
  instances in C++ extensions. Rather than `BoxedValue(new Foo(...))`, you must
  now use `BoxedValue::New<Foo>(...)`. This breaks all existing extensions that
  instantiate new values.

* **[breaking]** Removes all passing of `Var_self` in C++ extensions. Use
  `VAR_SELF` where `Var_self` was previously used, and delete the `Var_self`
  argument from function signatures. This breaks all existing extensions.

### Language

* **[fix]** Fixes obscure memory leak in `weak` values, triggered by a very
  specific sequence of events between three threads. This leak was unlikely to
  affect any programs that did not specifically attempt to trigger it.

* **[new]** Allows `update` blocks for `traverse` loops.

### Libraries

* **[new]** Adds `SpinlockMutex` to `lib/util`, for more efficient locking when
  lockouts are rare or extremely short.

## 0.18.1.0  -- 2021-10-19

### Language

* **[fix]** Fixes issue with explicit return discards skipping generation of
  function calls. Previously, `_ <- foo()` skipped generating the call to `foo`.

### Libraries

* **[new]** Adds `RandomExponential`, `RandomGaussian`, and `RandomUniform` to
  `lib/math` for generating random `Float` values.

* **[new]** Adds `Realtime.sleepSecondsPrecise` function to allow sleeps that
  are more precise than the kernel's latency.

* **[new]** Adds `checkGreaterThan` and `checkLessThan` to `Testing` in
  `lib/testing`.

## 0.18.0.0  -- 2021-08-01

### Compiler CLI

* **[breaking]** Creates a single shared library per Zeolite module, and uses
  dynamic linking as the default for binaries and `testcase`s. `--fast` mode
  still uses static linking, and static linking can be enabled for a binary
  using the `link_mode:` field in `.zeolite-module`:

  ```text
  root: "../.."
  path: "your/module"
  private_deps: [
    "lib/util"
  ]
  mode: binary {
    category: YourProgram
    function: run
    link_mode: static
  }
  ```

* **[breaking]** Eliminates dynamic memory allocation for `Bool`, `Char`,
  `Float`, and `Int`, including `optional` and `weak` variants. This improves
  the efficiency of code that makes a lot of function calls using those types.

  Note that when used as variable types, those types *already* avoided dynamic
  allocation. This therefore only applies to passing them in function calls, and
  using them with `optional` and `weak`.

  This breaks *all* existing C++ extensions:

  - `S<TypeValue>` must be replaced with `BoxedValue`.
  - Calls to `S_get` (only for `ValueType`) must be replaced with calls to the
    `BoxedValue` constructor.
  - `W<TypeValue>` must be replaced with `WeakValue`.
  - Calls to `W_get` must be replaced with calls to the `WeakValue` constructor.
  - Calls to `->AsBool()`, `->AsChar()`, `->AsFloat()`, `->AsInt()`,
    `->AsCharBuffer()`, and `->AsString()` must now use `.` instead of `->`.
  - `Present`, `Require`, and `Strong` are now in `BoxedValue::` instead of in
    `TypeValue::`.

  (Running `zeolite --templates` again will provide the correct type
  signatures and local argument aliasing.)

* **[breaking]** Updates `zeolite -c` (library mode) and `zeolite -m` (binary
  mode) to generate a new `.zeolite-module` and exit. Previously, both modes
  would create `.zeolite-module` and then attempt to compile it. The purpose of
  this change is to make users more aware that `.zeolite-module` can/should be
  edited, rather than attempting to generate a "correct" config entirely from
  CLI flags.

* **[new]** Updates recompile mode (`zeolite -r`) to reorder modules explicitly
  specified in the command based on dependencies. For example, in the command
  `zeolite -r foo bar`, if `foo` depends on `bar`, `bar` will be compiled first.
  Previously, modules would be compiled in the order they were specified.

  Note that this *will not* account for *indirect* dependencies unless recursive
  mode (`-R`) is used. This is because dealing with the intermediate dependency
  would be ambiguous when not recompiling recursively.

* **[behavior]** Even more optimization for function calls using 4 or fewer
  arguments, returns, or function params.

* **[behavior]** Updates the `fail` built-in to use `SIGTERM` instead of
  `SIGABRT` to terminate the process, to prevent core dumps when `fail` is used
  to signal an unhandled error.

### Language

* **[breaking]** Adds the `Bounded` `@type interface` for getting the
  `minBound()` and `maxBound()` of a type, and implements it for `Int` and
  `Char`. (This is breaking because `Bounded` is now reserved.)

* **[breaking]** Makes `Char` unsigned when converting to `Int` using `asInt()`.
  Previously, the value was signed, i.e., between -128 and 127. This change
  makes the `Int` values line up better with hex and oct `Char` escapes. The
  conversion from `Int` to `Char` (with `asChar()`) *is not* affected.

* **[new]** Updates param inference (e.g., `call<?>(foo)`) to handle param
  filters containing multiple inferred params. Previously, if a function had a
  filter such as `#x requires #y` and both `#x` and `#y` were inferred,
  inference would fail because params were inferred one at a time.

* **[new]** Adds the `timeout` field to `testcase`, to specify the number of
  seconds to allow each `unittest` to run. The default is `30`, which should be
  more than enough for most individual tests. The primary purpose of setting a
  default is to prevent a deadlock or infinite loop from blocking continuous
  integration (CI) runners.

* **[new]** Updates parsing to allow calling functions on value intializers, and
  `Bool`, `Char`, and `String` literals without needing `()`. For example,
  `Type{ foo }.bar()`, `true.asInt()`, `'0'.asInt()`, `"abc".defaultOrder()`.
  `Int` and `Float` literals still require `()`; otherwise, the use of `.` would
  be ambiguous.

### Libraries

* **[new]** Adds `ParseChars` helper to `lib/util`, to parse `Float` and `Int`
  from `String`s.

* **[new]** Adds `monoSeconds()` to `Realtime` in `lib/util`, to get a
  monotonic wall time in seconds.

## 0.17.0.0  -- 2021-04-06

### Language

* **[breaking]** Adds `CharBuffer`, as a random-access, mutable `Char` buffer,
  and makes `WriteAt` a built-in type. (This is breaking because `CharBuffer`
  and `WriteAt` are now reserved.)

* **[fix]** Fixes `reduce<String,Container>`, which previously failed.

* **[fix]** Fixes a race condition in function dispatching that was thought to
  be fixed previously.

* **[behavior]** Improves the memory efficiency of function calls that use 4 or
  fewer arguments, returns, or function params by reusing memory previously
  allocated for function calls.

### Libraries

* **[new]** Additions to `lib/util`:

  * **[new]** Adds `Ranges`, for simple min/max determinations.

  * **[new]** Adds `revZeroIndexed` to `Counter`, to create a reversed sequence
    of zero-indexed container indices.

  * **[new]** Adds `Reversed`, `BySize`, and `AlwaysEqual` comparators, to
    modify `LessThan` and `Equals` behaviors for objects.

  * **[new]** Adds `OrderH` and `ReadAtH`, to extend `LessThan` and `Equals`
    comparisons to simple containers.

* **[new]** Additions to `lib/container`:

  * **[new]** Adds `Sorting`, to support sorting of random-access containers.

  * **[new]** Adds `TreeSet`, to manage set membership of any sortable type.

  * **[new]** Adds `AutoBinaryTree`, to provide binary search tree (BST)
    functionality to custom containers that need to augment each node with
    additional data.

  * **[new]** Implements `Container.size()` in `SearchTree`.

  * **[new]** Adds `KeyValueH`, to extend `LessThan` and `Equals` comparisons to
    `KeyValue`.

* **[new]** Adds `CategoricalTree` to `lib/math`, as a dynamic representation
  of a [categorical distribution][categorical] over objects of any sortable type.

## 0.16.1.0  -- 2021-04-02

### Language

* **[fix]** Fixes a regression where a race condition in function dispatching
  would cause an error saying that a category does not implement the function
  that was being called.

* **[behavior]** Optimizes calls to `@type` functions when made from `@value`
  functions of the same type.

* **[behavior]** Optimizes `@value` initialization (e.g., `Type{ }`) when done
  from `@type` functions of the same type.

### Libraries

* **[new]** Moves `Mutex`, `SimpleMutex`, and `MutexLock` from `lib/thread`
  to `lib/util`. This is because they provide basic concurrency protection to
  objects that should not need to depend on `-lpthread`.

  Note that this change *is not* breaking because `lib/util` was already a
  public dependency of `lib/thread`. In other words, if a module already used
  `lib/thread` then it got `lib/util` for free. This means that modules only
  needing mutex functionality from `lib/thread` can instead use `lib/util`.

* **[new]** Adds `getForward` and `getReverse` to `SearchTree`, to allow the
  caller to iterate after looking up an entry.

* **[new]** Adds `checkEmpty`, `checkPresent`, and `checkOptional` to `Testing`
  in `lib/testing`.

## 0.16.0.0  -- 2021-04-01

### Language

* **[breaking]** Disallows param filters in `@value interface` and
  `@type interface`. Param filters are only used to allow procedures to call
  `@type` and `@value` functions. Previously, they were enforced in all
  situations where param substitution occurred, but this is no longer the case.
  (See further down in this list.)

* **[breaking]** Removes support for internal type params for values. This was
  an obscure feature that was never documented and was only used in some
  integration tests that had not been touched in over 2 years. This breaks most
  hand-written C++ extensions, but they can be fixed by removing the
  `ParamTuple` argument passed to the base-class constructor in `ExtValue_Foo`
  for C++ extension `Foo`.

* **[breaking]** Adds the `traverse` built-in syntax, which automatically
  iterates over a container that exposes the (new) `Order<|#x>`
  `@value interface`. (This is breaking because `traverse`, `Order`, and
  `DefaultOrder` are now reserved.)

  ```text
  traverse (container.defaultOrder() -> Foo value) {
    // executed once per Foo in the container
  }
  ```

* **[breaking]** Marks `@category` member variables as read-only when
  initializing other member variables. Essentially, this just means that you can
  no longer do something like this:

  ```text
  @category Int foo <- 1
  @category Int bar <- (foo <- 2)
  ```

* **[new]** Removes the requirement that type instances (e.g., `Type<Int>`)
  satisfy the requirements of the type's parameter filters when not being used
  in a function call.

  ```text
  concrete Type<|#x> {
    #x requires Foo
  }
  ```

  In the example above, `Type<Bar>` was universally disallowed if `Bar` could
  not be converted to `Foo`. The new behavior is to enforce that restriction
  only when calling `@type` functions, initializing values (e.g.,
  `Type<#x>{ }`), and using the type as a param in a function call.

  Since `#x` is covariant in this example, you could convert a `Type<Bar>` to a
  `Type<any>`, even though `any` is not a child of `Foo`.

* **[new]** Removes variance checks for category-level param filters. Variance
  was originally *not* checked, but then the checks were added prior to version
  `0.1.0.0` for obscure mathematical reasons that no longer make sense.

  In particular, filters such as `#x defines Equals<#x>` *were not* previously
  allowed except when `#x` was invariant, but now it is allowed for all
  variances.

  The revised perspective is that param filters form a meta-type for param
  substitutions as inputs, with the constructed type as the output. Once the
  output is received, the input constraints are no longer recoverable.

  Note that category-level param variance is still checked when params are used
  on the right side of filters for function params. This is because not doing so
  could result in bad argument and return types when inheriting functions.

  Params also still cannot have *both* upper and lower bounds at the same time.

* **[new]** When inferring type parameters for function calls (e.g.,
  `call<?>(foo)`), allows `all` to be a valid lower bound and `any` to be a
  valid upper bound. Previously, an inferred type of `all` in covariant
   positions and `any` in contravariant positions would cause inference to fail.

* **[new]** Allows `$ReadOnly[...]$` and `$Hidden[...]$` for member variables in
  definitions of `concrete` categories.

* **[new]** Makes `Argv` available to all threads. Previously, it was only
  available to the main thread. (Even though `Argv` is exposed via `lib/util`,
  this change also affects C++ extensions that use `Argv` from `logging.hpp`.)

* **[fix]** Fixes regression with function names used in stack traces.
  Previously, if a function was inherited and the types were not overridden,
  the category name in the trace would be that of the parent, rather than that
  of the one defining the procedure.

### Compiler CLI

* **[breaking]** Removes the default compiler mode when calling `zeolite`. The
  previous default was `-c`, which creates a new `.zeolite-module` for
  incremental library compilation. The default was removed because accidentally
  typing `zeolite -f foo` instead of `zeolite -t foo` would overwrite the
  `.zeolite-module` for `foo` rather than running its tests.

* **[breaking]** Updates how `$TraceCreation$` is implemented in C++. This only
  breaks hand-written C++ extensions that use the `CAPTURE_CREATION` macro,
  which now must pass a string-literal category name as a macro argument,
  e.g., `CAPTURE_CREATION("Foo")`.

* **[new]** Adds the `--log-traces` option for testing mode (`zeolite -t`) to
  capture a record of every line of Zeolite code executed when running tests.

* **[fix]** Fixes dependency resolution for C++ extensions that define
  `$ModuleOnly$` categories.

* **[behavior]** Improves the efficiency of call dispatching and `reduce` calls
  for categories that inherit a lot of `interface`s. This is unlikely to affect
  performance when a category has less than 10 direct or indirect parent
  `interface`s.

### Libraries

* **[new]** Adds `ThreadCondition` to `lib/thread`, which allows threads to wait
  for an arbitrary condition signaled by another thread. (Similar to
  `pthread_cond_t` in POSIX.)

* **[new]** Adds `EnumeratedBarrier` to `lib/thread`, which can be use to
  synchronize a fixed number of threads. (Similar to `pthread_barrier_t` in
  POSIX, but with strict validation to help prevent deadlocks.)

* **[new]** Adds `Realtime` to `lib/thread`, to allow threads to sleep based on
  wall time.

* **[new]** Adds `Counter` and `Repeat` to `lib/util`, to help create sequences
  for use with the new `traverse` built-in syntax.

* **[new]** Implements forward and reverse iteration to `SearchTree` in
  `lib/container`.

### Examples

* Adds `example/primes`, as a demonstration of multithreading.

## 0.15.0.0  -- 2021-03-24

### Language

* **[breaking]** Splits `Builder<#x>` built-in into `Append<#x|>` and
  `Build<|#x>` to more accurately reflect its semantics.

* **[breaking]** Splits `ReadPosition<#x>` built-in into `ReadAt<|#x>`,
  `SubSequence`, and `Container` to be more concise, and to split up reading
  from subsequencing. This also includes renaming `readSize()` to just `size()`.

* **[new]** Adds the `Default` `@type interface`, to specify a default `@value`
  for the type.

* **[fix]** Fixes merging of duplicate inherited categories. Previously, if a
  category `Foo` was indirectly inherited multiple times then the compiler
  would complain about having multiple functions with the same name.

* **[fix]** Adds support for `^` with the `Bool` built-in type.

### Libraries

* **[breaking]** Refactors iterators in `lib/util`:

  * **[breaking]** Makes `ReadIterator` a `@value interface`.

  * **[breaking]** Adds `AutoReadIterator`, `AutoWriteIterator`, and
    `AutoIterator` helpers to create iterators from `ReadAt` and `WriteAt`.
    (`AutoReadIterator` replaces the previous `ReadIterator`.)

  * **[new]** Adds `WriteAt`, `WriteCurrent`, `WriteIterator`, and `Iterator`
    `@value interface`s.

* **[new]** Adds `lib/container`:

  * **[new]** `Vector`, a random-access container.

  * **[new]** `SearchTree`, a binary search tree.

  * **[new]** `TypeMap`, a heterogeneous key-value map of different value types.

  * **[new]** Various `@value interface`s with container usage patterns.

* **[new]** Adds `lib/thread` (must be built manually):

  * **[new]** `ProcessThread`, a basic thread runner.

  * **[new]** `SimpleMutex` and `MutexLock`, for basic mutex logic.

  * **[new]** Various `@value interface`s with thread usage patterns.

### Compiler CLI

* **[breaking]** Removes implicit dependence of C++ extensions on built-in
  categories `Bool`, `Float`, `Char`, `Int`, `String`, and `Formatted`.
  Extensions that use these categories *without* using them in the corresponding
  `concrete` declaration will need to add them to the `requires:` field of the
  `category_source` in `.zeolite-module`.

  ```text
  category_source {
    source: "Extension_MyType.cpp"
    categories: [MyType]
    requires: [String,Formatted]
  }
  ```

* **[breaking]** Cleans up `TypeInstance` caching for C++ extensions with
  parameters. See `lib/container/src/Extension_Vector.cpp` for an example of the
  new semantics, or use `--templates` to create a new template.

### Examples

* Removes `example/tree`. All of the corresponding code has been moved into
  `lib/container`.

## 0.14.0.0  -- 2021-03-19

### Language

* **[breaking]** Adds the `#self` meta-type. This is an implicit type param that
  is dynamically replaced with the type of the object a function is called on.
  For example, if a `@value interface` function has a return type of `#self`
  then any implementation must return an object of *its own type* from that
  function. (This is a breaking change because it reserves the param name
  `#self`.)

### Libraries

* **[breaking]** Removes the type parameters from `IterateForward` and
  `IterateReverse` in `lib/util`. This was made possible by the addition of the
  `#self` meta-type.

## 0.13.0.0  -- 2021-03-17

### Language

* **[new]** Adds pragmas for local variable rules:

  * **[new]** `$ReadOnly[foo]$` marks `foo` as read-only in the current context
    following the line with the pragma.

  * **[new]** `$Hidden[foo]$` hides `foo` in the current context following the
    line with the pragma.

* **[fix]** Fixes a latent issue with the `reduce` built-in when converting an
  intersection type to a union type, when one or both sides contains another
  nested union or intersection. This was previously fixed for compile-time
  checks, but `reduce` was missed.

### Compiler CLI

* **[breaking]** Streamlines C++ extensions to cut down on boilerplate code.
  This is a massive change that breaks *all* existing C++ extensions; however,
  hand-written procedure definitions can be copied-and-pasted into a new
  extension template. Use `zeolite --templates` to regenerate new templates for
  C++ extensions.

* **[breaking]** Adds [`microlens`][microlens] and
  [`microlens-th`][microlens-th] as dependencies, to help clean up the compiler
  code. This should not change the compiler's behavior.

## 0.12.0.0  -- 2020-12-11

### Language

* **[breaking]** Several more fixes for named returns:

  * **[new]** Allows *initialization* of named returns in `if`/`elif`/`while`
    predicates. Although *setting* a named return in a predicate has always been
    allowed, it previously did not count as an initialization for the purposes
    of checking usage in subsequent statements.

  * **[breaking]** Fixes a bug where named returns used in `if`/`elif`/`while`
    predicates inside of `cleanup` blocks were not getting checked for
    initialization. This turns a potential runtime error into a compile-time
    error, which could break buggy code that was never going to be executed.

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

* **[breaking]** Updates associativity of infix operators:

  * **[breaking]** Makes infix `Bool` operators `&&` and `||`
    *right-associative*. For example, `x && y && z` used to be parsed as
    `(x && y) && z`, whereas now it is parsed as `x && (y && z)`. This better
    reflects the intuition of side-effects happening in order, with respect to
    short-circuiting. (No other operators currently short-circuit.)

  * **[breaking]** Makes comparison operators (e.g., `<=`) *non-associative*.
    This means that expressions such as `x < y == true` will no longer compile.
    This is because an equivalent-looking expression `true == x < y` might have
    a different interpretation. (Note that order operators such as `<` *are not*
    defined for `Bool`, so something like `x < y < z` would have already failed
    previously.)

  All other infix operators (including infix function calls such as
  ``x `Math.pow` y``) remain *left-associative*.

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

[categorical]: https://en.wikipedia.org/wiki/Categorical_distribution
[megaparsec]: https://hackage.haskell.org/package/megaparsec
[microlens]: https://hackage.haskell.org/package/microlens
[microlens-th]: https://hackage.haskell.org/package/microlens-th
[parsec]: https://hackage.haskell.org/package/parsec
