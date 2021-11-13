# [Zeolite Programming Language][zeolite]

[![Haskell CI Status][action-status]][action-zeolite]
[![Hackage Status][hackage-status]][hackage-zeolite-lang]

Zeolite is a statically-typed, general-purpose programming language. The type
system revolves around defining objects and their usage patterns.

Zeolite prioritizes making code written with it simple to understand for readers
who didn't write the original code. This is done by limiting flexibility in
some places and increasing it in others. In particular, emphasis is placed on
the user's experience when troubleshooting code that is *incorrect*.

The design of the type system and the language itself is influenced by positive
and negative experiences with Java, C++, Haskell, Python, and Go, with
collaborative development, and with various systems of code-quality enforcement.

Due to the way GitHub renders embedded HTML, the colors might not show up in the
syntax-highlighted code in this document. If you use the Chrome browser, you can
view the intended formatting using the
[Markdown Viewer](https://chrome.google.com/webstore/detail/markdown-viewer/ckkdlimhmcjmikdlpkmbgfkaikojcbjk)
extension to view the
[raw version](https://raw.githubusercontent.com/ta0kira/zeolite/master/README.md)
of this document.

## Table of Contents

- [Project Status](#project-status)
- [Language Overview](#language-overview)
  - [Programming Paradigms](#programming-paradigms)
  - [Parameter Variance](#parameter-variance)
  - [Parameters as Variables](#parameters-as-variables)
  - [Integrated Test Runner](#integrated-test-runner)
  - [Integrated Build System](#integrated-build-system)
  - [Data Encapsulation](#data-encapsulation)
- [Quick Start](#quick-start)
  - [Installation](#installation)
  - [Hello World](#hello-world)
- [Writing Programs](#writing-programs)
  - [Basic Ideas](#basic-ideas)
  - [Declaring Functions](#declaring-functions)
  - [Defining Functions](#defining-functions)
    - [Using Variables](#using-variables)
    - [Calling Functions](#calling-functions)
    - [Functions As Operators](#functions-as-operators)
    - [Data Members and Value Creation](#data-members-and-value-creation)
    - [Conditionals](#conditionals)
    - [Scoping and Cleanup](#scoping-and-cleanup)
    - [Loops](#loops)
    - [Multiple Returns](#multiple-returns)
    - [Optional and Weak Values](#optional-and-weak-values)
    - [Deferred Variable Initialization](#deferred-variable-initialization)
  - [Using Parameters](#using-parameters)
  - [Using Interfaces](#using-interfaces)
  - [Immutable Types](#immutable-types)
  - [The `#self` Parameter](#the-self-parameter)
  - [Type Inference](#type-inference)
  - [Other Features](#other-features)
    - [Meta Types](#meta-types)
    - [Runtime Type Reduction](#runtime-type-reduction)
  - [Builtins](#builtins)
    - [Reserved Words](#reserved-words)
    - [Builtin Types](#builtin-types)
    - [Builtin Constants](#builtin-constants)
    - [Builtin Functions](#builtin-functions)
- [Layout and Dependencies](#layout-and-dependencies)
  - [Using Public Source Files](#using-public-source-files)
  - [Standard Library](#standard-library)
  - [Modules](#modules)
- [Unit Testing](#unit-testing)
  - [Writing Tests](#writing-tests)
  - [Code Coverage](#code-coverage)
- [Compiler Pragmas and Macros](#compiler-pragmas-and-macros)
  - [Source File Pragmas](#source-file-pragmas)
  - [Procedure Pragmas](#procedure-pragmas)
  - [Local Variable Rules](#local-variable-rules)
  - [Expression Macros](#expression-macros)
- [Known Language Limitations](#known-language-limitations)

## Project Status

Zeolite is still evolving in all areas (syntax, build system, etc.), and it
still lacks a lot of standard library functionality. That said, it was designed
with practical applications in mind. It *does not* prioritize having
impressive toy examples (e.g., merge-sort or "Hello World" in one line); the
real value is seen in programs with higher relational complexity.

## Language Overview

This section discusses some of the features that make Zeolite unique. It does
not go into detail about all of the language's features; see
[Writing Programs](#writing-programs) and the [full examples][examples] for more
specific language information.

### Programming Paradigms

Zeolite currently uses both [procedural][procedural] and [object-oriented][oop]
programming paradigms. It shares many features with Java, but it also has
additional features and restrictions meant to simplify code maintenance.

### Parameter Variance

The initial motivation for Zeolite was a type system that allows implicit
conversions between different parameterizations of parameterized types. A
parameterized type is a type with type "place-holders", e.g., `template`s in C++
and generics in Java.

Java and C++ *do not* allow you to safely convert between different
parameterizations. For example, you cannot safely convert a `List<String>` into
a `List<Object>` in Java. This is primarily because `List` uses its type
parameter for both input and output.

Zeolite, on the other hand, uses [declaration-site variance][variance] for each
parameter. (C# also does this to a lesser extent.) This allows the language to
support very powerful recursive type conversions for parameterized types.
Zeolite *also* allows [use-site variance][variance] declarations, like Java
uses.

Building variance into the core of the type system also allows Zeolite to have a
[special meta-type](#the-self-parameter) that interfaces can use to require that
implementations return a value of *their own type* rather than *the type of the
interface*. This is particularly useful for defining interfaces for iterators
and builders, whose methods often perform an update and return a value of the
same type.

### Parameters as Variables

Zeolite treats type parameters both as type place-holders (like in C++ and
Java) and as *type variables* that you can call functions on. This further
allows Zeolite to have interfaces that declare functions that operate on *types*
in addition to interfaces that declare functions that operate on *values*. (This
would be like having `abstract static` methods in Java.)

This helps solve a few separate problems:

- Operations like `equals` comparisons in Java are always dispatched to the
  *left* object, which could lead to inconsistent results if the objects are
  swapped: `foo.equals(bar)` is not the same as `bar.equals(foo)`. This
  dispatching asymmetry can be eliminated by making `equals` a type function
  (e.g., `MyType.equals(foo,bar)`), and further creating an interface that
  requires implementations to support such calls.

- Factory patterns can be abstracted out into interfaces. For example, you could
  create a factory interface that requires an implementation to parse a new
  object from a `String`, without needing to instantiate the factory object
  itself. You could just implement the factory function directly in `MyType`,
  without needing a separate `MyTypeFactory`.

### Integrated Test Runner

The major advantage of statically-typed programming languages is their
compile-time detection of code that *should not* be allowed. On the other hand,
there is a major testability gap when it comes to ensuring that your
statically-typed code *disallows* what you expect it to.

Zeolite has a special source-file extension for unit tests, and a built-in
compiler mode to run them.

- Tests can check for runtime success, compilation success, compilation failure,
  and even crashes. Normally you would need a third-party test runner to check
  for required compilation failures and crashes.

- The test mode includes a command-line option to collect code-coverage data,
  which can be critical for determining test efficacy.

Nearly all of the integration testing of the Zeolite language itself is done
using this feature, but it is also supported for general use with Zeolite
projects.

### Integrated Build System

The Zeolite compiler supports a module system that can incrementally compile
projects without the user needing to create build scripts or `Makefile`s.

- Modules are configured via a simple config file.
- File-level and symbol-level imports and includes *are not* necessary, allowing
  module authors to freely rearrange file structure.
- Type dependencies are automatically resolved during linking so that output
  binaries contain only the code that is relevant.
- Module authors can back Zeolite code with C++.
- The module system is integrated with the compiler's built-in testing mode.

This means that the programmer can focus on code rather than on build rules, and
module authors can avoid writing verbose build instructions for the users of
their modules.

### Data Encapsulation

The overall design of Zeolite revolves around data encapsulation:

- **No default construction or copying.** This means that objects can only be
  created by explicit factory functions. (A very common mistake in C++ code is
  forgetting to *disallow or override* default construction or copying.) This
  also means that accidental deep copying is not even possible in Zeolite.

- **Only abstract interfaces can be inherited.** Types that define procedures or
  contain data members *cannot* be further extended. This encourages the
  programmer to think more about usage patterns and less about data
  representation when designing interactions between types.

- **No "privileged" data-member access.** No object has *direct* access to the
  data members of any other object; not even other objects of the same type.
  This forces the programmer to also think about usage patterns when dealing
  with other objects of the same type.

- **No `null` values.** Every variable must be explicitly initialized. This
  obviates questions of whether or not a variable actually contains a value.
  When combined with the elimination of default construction, this means that a
  variable can only hold values that a type author has specifically allowed.

  Zeolite has an `optional` storage modifier for use in a variable's type, but
  it creates a new and incompatible type, whose value cannot be used without
  first extracting it. This is in contrast to Java allowing any variable to be
  `null`. (It is more like `Optional` in Java 8, but as built-in syntax, and
  with nesting disallowed.)

- **No reflection or down-casting.** The only thing that has access to the
  "real" type of a value is the value itself. This means that the reflection and
  down-casting tricks available in Java to circumvent the compiler *are not*
  available in Zeolite. (User-defined types can selectively allow "down-casting
  like" semantics by reasoning about
  [parameters as variables](#parameters-as-variables), however.)

- **Implementation details are kept separate.** In Zeolite, only *public
  inheritance* and *public functions* show up where an object type is declared,
  to discourage relying on implementation details of the type.

  C++ and Java allow (and in some cases *require*) implementation details (data
  members, function definitions, etc.) to show up in the same place as the
  user-accessible parts of a `class`. The result is that the user of the `class`
  will often rely on knowledge of how it works internally.

Although all of these limitations preclude a lot of design decisions allowed in
languages like Java, C++, and Python, they also drastically reduce the possible
complexity of inter-object interactions. Additionally, they generally *do not*
require ugly work-arounds; see the [full examples][examples].

## Quick Start

### Installation

Requirements:

- A Unix-like operating system. Zeolite has been tested on Linux and FreeBSD,
  but in theory it should also work on OpenBSD, NetBSD, OS X, etc.
- A Haskell compiler such as [`ghc`][ghc] that can install packages using
  [`cabal`][cabal], as well as the [`cabal`][cabal] installer.
- A C++ compiler such as [`clang++`][clang] or [`g++`][gcc] and the standard
  `ar` archiver present on most Unix-like operating systems.

If you use a modern Linux distribution, most of the above can be installed using
the package manager that comes with your distribution.

Once you meet all of those requirements, follow the installation instructions
for the [**`zeolite-lang`**][hackage-zeolite-lang] package on
[Hackage][hackage]. Please take a look at the [issues page][zeolite-issues] if
you run into problems.

If you happen to use the [`kate`][kate] text editor, you can use the syntax
highlighting in [`zeolite.xml`][zeolite.xml].

### Hello World

It's the [any%](https://en.wiktionary.org/wiki/any%25) of programming.

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<span style='color:#898887;'>// hello-world.0rx</span>

<b>concrete</b> <b><span style='color:#0057ae;'>HelloWorld</span></b> {
  <span style='color:#644a9b;'>@type</span> run () -&gt; ()
}

<b>define</b> <b><span style='color:#0057ae;'>HelloWorld</span></b> {
  run () {
    <span style='color:#006e28;'>\</span> <span style='color:#0057ae;'>LazyStream</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>Formatted</span></i><span style='color:#c02040;'>&gt;</span><span style='color:#644a9b;'>.</span>new()
        .append(<span style='color:#bf0303;'>&quot;Hello World</span><span style='color:#924c9d;'>\n</span><span style='color:#bf0303;'>&quot;</span>)
        .writeTo(<span style='color:#0057ae;'>SimpleOutput</span><span style='color:#644a9b;'>.</span>stderr())
  }
}</pre>

```shell
# Compile.
zeolite -I lib/util --fast HelloWorld hello-world.0rx

# Execute.
./HelloWorld
```

Also see some [full examples][examples] for more complete feature usage.

## Writing Programs

This section breaks down the separate parts of a Zeolite program. See the
[full examples][examples] for a more integrated language overview.

### Basic Ideas

Zeolite programs use object-oriented and procedural programming paradigms.
**Type categories** are used to define object types, much like `class`es in
Java and C++. They *are not* called "classes", just to avoid confusion about
semantic differences with Java and C++.

All type-category names start with an uppercase letter and contain only letters
and digits.

All procedures and data live inside `concrete` type categories. Every program
must have at least one `concrete` category with the procedure to be executed
when the program is run.

`concrete` categories are split into a **declaration** and a **definition**.
Code for both should be in files ending with `.0rx`. (The `.0rp` file type
contains only declarations, and will be discussed later.)

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<span style='color:#898887;'>// myprogram/myprogram.0rx</span>

<span style='color:#898887;'>// This declares the type.</span>
<b>concrete</b> <b><span style='color:#0057ae;'>MyProgram</span></b> {
  <span style='color:#898887;'>// The entry point must be a () -&gt; () function. This means that it takes no</span>
  <span style='color:#898887;'>// arguments and returns no arguments. (@type will be discussed later.)</span>
  <span style='color:#644a9b;'>@type</span> run () -&gt; ()
}

<span style='color:#898887;'>// This defines the type.</span>
<b>define</b> <b><span style='color:#0057ae;'>MyProgram</span></b> {
  run () {
    <span style='color:#898887;'>// ...</span>
  }
}</pre>

**IMPORTANT:** All programs or modules must be in their own directory so that
`zeolite` is able to cache information about the build. Unlike some other
compilers, you *do not* specify all command-line options every time you
recompile a binary or module.

```shell
# Create a new .zeolite-module config. (Only once!)
zeolite -m MyProgram myprogram

# Recompile the module and binary. (After any config or code updates.)
# All sources in myprogram will be compiled. -m MyProgram selects the entry
# point. The default output name for the binary here is myprogram/MyProgram.
zeolite -r myprogram

# Execute.
myprogram/MyProgram
```

```shell
# An alternative, if you only have one .0rx and want to quickly iterate.
zeolite --fast MyProgram myprogram/myprogram.0rx
```

### Declaring Functions

A **function declaration** specifies the **scope** of the function and its
**argument** and **return** types. (And optionally type parameters and parameter
filters, to be discussed later.) The declaration simply indicates the existence
of a function, without specifying its behavior.

All function names start with a lowercase letter and contain only letters and
digits.

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<b>concrete</b> <b><span style='color:#0057ae;'>MyCategory</span></b> {
  <span style='color:#898887;'>// @value indicates that this function requires a value of type MyCategory.</span>
  <span style='color:#898887;'>// This function takes 2x Int and returns 2x Int.</span>
  <span style='color:#644a9b;'>@value</span> minMax (<i><span style='color:#0057ae;'>Int</span></i>,<i><span style='color:#0057ae;'>Int</span></i>) -&gt; (<i><span style='color:#0057ae;'>Int</span></i>,<i><span style='color:#0057ae;'>Int</span></i>)

  <span style='color:#898887;'>// @type indicates that this function operates on MyCategory itself. This is</span>
  <span style='color:#898887;'>// like a static function in C++.</span>
  <span style='color:#898887;'>// This function takes no arguments and returns MyCategory.</span>
  <span style='color:#644a9b;'>@type</span> create () -&gt; (<span style='color:#0057ae;'>MyCategory</span>)

  <span style='color:#898887;'>// @category indicates that this function operates on MyCategory itself. This</span>
  <span style='color:#898887;'>// is like a static function in Java. (The semantics of @category are similar</span>
  <span style='color:#898887;'>// to those of @type unless there are type parameters.)</span>
  <span style='color:#644a9b;'>@category</span> copy (<span style='color:#0057ae;'>MyCategory</span>) -&gt; (<span style='color:#0057ae;'>MyCategory</span>)
}</pre>

### Defining Functions

Functions are defined in the category definition. They *do not* need to repeat
the function declaration; however, they can do so in order to refine the
argument and return types for internal use.

The category definition can also declare *additional* functions that are not
visible externally.

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<b>concrete</b> <b><span style='color:#0057ae;'>MyCategory</span></b> {
  <span style='color:#644a9b;'>@type</span> minMax (<i><span style='color:#0057ae;'>Int</span></i>,<i><span style='color:#0057ae;'>Int</span></i>) -&gt; (<i><span style='color:#0057ae;'>Int</span></i>,<i><span style='color:#0057ae;'>Int</span></i>)
}

<b>define</b> <b><span style='color:#0057ae;'>MyCategory</span></b> {
  <span style='color:#898887;'>// minMax is defined here.</span>
  minMax (x,y) {
    <b>if</b> (superfluousCheck(x,y)) {
      <b>return</b> x, y
    } <b>else</b> {
      <b>return</b> y, x
    }
  }

  <span style='color:#898887;'>// superfluousCheck is only available inside of MyCategory.</span>
  <span style='color:#644a9b;'>@type</span> superfluousCheck (<i><span style='color:#0057ae;'>Int</span></i>,<i><span style='color:#0057ae;'>Int</span></i>) -&gt; (<i><span style='color:#0057ae;'>Bool</span></i>)
  superfluousCheck (x,y) {
    <b>return</b> x &lt; y
  }
}</pre>

All arguments must either have a unique name or be ignored with `_`.

`@value` functions have access to a special constant **`self`**, which refers to
the object against which the function was called.

#### Using Variables

**Variables** are assigned with `<-` to indicate the direction of assignment.
*Every* variable must be initialized; there are no `null` values in Zeolite.
(However, see `optional` later on.)

All variable names start with a lowercase letter and contain only letters and
digits. When a location is needed for assignment (e.g., handling a function
return, taking a function argument), you can use `_` in place of a variable
name to ignore the value.

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<span style='color:#898887;'>// Initialize with a literal.</span>
<i><span style='color:#0057ae;'>Int</span></i> value &lt;- <span style='color:#b08000;'>0</span>

<span style='color:#898887;'>// Initialize with a function result.</span>
<i><span style='color:#0057ae;'>Int</span></i> value &lt;- getValue()</pre>

Unlike other languages, Zeolite *does not* allow variable masking. For example,
if there is already a variable named `x` available, you *cannot* create a new
`x` variable even in a smaller scope.

All variables are **shared** and their values *are not* scoped like they are in
C++. You should not count on knowing the lifetime of any given value.

#### Calling Functions

Return values from **function calls** must *always* be explicitly handled by
assigning them to a variable, passing them to another function or ignoring them.
(This is required even if the function does not return anything, primarily to
simplify parsing.)

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<span style='color:#898887;'>// Utilize the return.</span>
<i><span style='color:#0057ae;'>Int</span></i> value &lt;- getValue()

<span style='color:#898887;'>// Explicitly ignore a single value.</span>
<b>_</b> &lt;- getValue()

<span style='color:#898887;'>// Ignore all aspects of the return.</span>
<span style='color:#898887;'>// (Prior to compiler version 0.3.0.0, ~ was used instead of \.)</span>
<span style='color:#006e28;'>\</span> printHelp()</pre>

- Calling a function with `@value` scope requires a value of the correct type,
  and uses `.` notation, e.g., `foo.getValue()`.
- Calling a function with `@type` scope requires the type with parameter
  substitution (if applicable), and uses `.` notation, e.g.,
  `MyCategory<Int>.create()`. (Prior to compiler version `0.9.0.0`, `$` was used
  instead of `.`.)
- Calling a function with `@category` scope requires the category itself, and
  uses the `:` notation, e.g., `MyCategory:foo()`. (Prior to compiler version
  `0.9.0.0`, `$$` was used instead of `:`.)
- You can skip qualifying function calls (e.g., in the example above) if the
  function being called is in the same scope or higher. For example, you can
  call a `@type` function from the procedure for a `@value` function in the
  same category.

The `fail` builtin can be used to immediately terminate the program. It *is not*
considered a function since it cannot return; therefore, you do not need to
precede it with `\`.

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<b>define</b> <b><span style='color:#0057ae;'>MyProgram</span></b> {
  run () {
    <b>fail</b>(<span style='color:#bf0303;'>&quot;MyProgram does nothing&quot;</span>)
  }
}</pre>

Functions *cannot* be overloaded like in Java and C++. Every function must have
a unique name. Functions inherited from different places can be explicitly
merged, however. This can be useful if you want interfaces to have overlapping
functionality without having an explicit parent for the overlap.

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>ForwardIterator</span></b><span style='color:#c02040;'>&lt;</span><span style='color:#c04040;'>|</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  next () -&gt; (<b>optional</b> <b>#self</b>)
  get () -&gt; (<i><span style='color:#0057ae;'>#x</span></i>)
}

<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>ReverseIterator</span></b><span style='color:#c02040;'>&lt;</span><span style='color:#c04040;'>|</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  prev () -&gt; (<b>optional</b> <b>#self</b>)
  get () -&gt; (<i><span style='color:#0057ae;'>#x</span></i>)
}

<b>concrete</b> <b><span style='color:#0057ae;'>Iterator</span></b><span style='color:#c02040;'>&lt;</span><span style='color:#c04040;'>|</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  <b>refines</b> <span style='color:#0057ae;'>ForwardIterator</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span>
  <b>refines</b> <span style='color:#0057ae;'>ReverseIterator</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span>

  <span style='color:#898887;'>// An explicit override is required in order to merge get from both parents.</span>
  get () -&gt; (<i><span style='color:#0057ae;'>#x</span></i>)
}</pre>

#### Functions As Operators

Zeolite allows some functions to be used as **operators**. This allows users to
avoid excessive parentheses when using named mathematical  functions.

Functions with two arguments can use **infix** notation. The operator precedence
is always between comparisons (e.g., `==`) and logical (e.g., `&&`).

Functions with one argument can use **prefix** notation. These are evaluated
strictly before all infix operators.

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<b>concrete</b> <b><span style='color:#0057ae;'>Math</span></b> {
  <span style='color:#644a9b;'>@type</span> plus (<i><span style='color:#0057ae;'>Int</span></i>,<i><span style='color:#0057ae;'>Int</span></i>) -&gt; (<i><span style='color:#0057ae;'>Int</span></i>)
  <span style='color:#644a9b;'>@type</span> neg (<i><span style='color:#0057ae;'>Int</span></i>) -&gt; (<i><span style='color:#0057ae;'>Int</span></i>)
}

<span style='color:#898887;'>// ...</span>

<span style='color:#898887;'>// Math.plus is evaluated first.</span>
<i><span style='color:#0057ae;'>Int</span></i> x &lt;- <span style='color:#b08000;'>1</span> <b><span style='color:#c02040;'>`</span></b><span style='color:#0057ae;'>Math</span><span style='color:#644a9b;'>.</span>plus<b><span style='color:#c02040;'>`</span></b> <span style='color:#b08000;'>2</span> * <span style='color:#b08000;'>5</span>
<span style='color:#898887;'>// Math.neg is evaluated first.</span>
<i><span style='color:#0057ae;'>Int</span></i> y &lt;- <b><span style='color:#c02040;'>`</span></b><span style='color:#0057ae;'>Math</span><span style='color:#644a9b;'>.</span>neg<b><span style='color:#c02040;'>`</span></b> x <b><span style='color:#c02040;'>`</span></b><span style='color:#0057ae;'>Math</span><span style='color:#644a9b;'>.</span>plus<b><span style='color:#c02040;'>`</span></b> <span style='color:#b08000;'>2</span></pre>

#### Data Members and Value Creation

Unlike Java and C++, there is no "default construction" in Zeolite. In addition,
Zeolite also lacks the concept of "copy construction" that C++ has. This means
that new values can only be created using a factory function. In combination
with required variable initialization, this ensures that the programmer never
needs to worry about unexpected missing or uninitialized values.

Data members are never externally visible; they only exist in the category
definition. Any access outside of the category must be done using
explicitly-defined functions.

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<b>concrete</b> <b><span style='color:#0057ae;'>MyCategory</span></b> {
  <span style='color:#644a9b;'>@type</span> create () -&gt; (<span style='color:#0057ae;'>MyCategory</span>)
}

<b>define</b> <b><span style='color:#0057ae;'>MyCategory</span></b> {
  <span style='color:#898887;'>// A data member unique to each MyCategory value.</span>
  <span style='color:#644a9b;'>@value</span> <i><span style='color:#0057ae;'>Int</span></i> value

  create () {
    <span style='color:#898887;'>// Initialization is done with direct assignment.</span>
    <b>return</b> <span style='color:#0057ae;'>MyCategory</span>{ <span style='color:#b08000;'>0</span> }
  }
}

<span style='color:#898887;'>// ...</span>

<span style='color:#898887;'>// Create a new value in some other procedure.</span>
<span style='color:#0057ae;'>MyCategory</span> myValue &lt;- <span style='color:#0057ae;'>MyCategory</span><span style='color:#644a9b;'>.</span>create()</pre>

There *is no syntax* for accessing a data member from another object; even
objects of the same type. This effectively makes all variables **internal**
rather than just `private` like in Java and C++. As long as parameter variance
is respected, you can provide access to an individual member with getters and
setters.

As of compiler version `0.14.0.0`, you can use [`#self`](#the-self-parameter) in
place of the full type when you are creating a value of the same type from a
`@type` or `@value` function.

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<b>concrete</b> <b><span style='color:#0057ae;'>MyCategory</span></b> {
  <span style='color:#644a9b;'>@type</span> create () -&gt; (<span style='color:#0057ae;'>MyCategory</span>)
}

<b>define</b> <b><span style='color:#0057ae;'>MyCategory</span></b> {
  <span style='color:#644a9b;'>@value</span> <i><span style='color:#0057ae;'>Int</span></i> value

  create () {
    <b>return</b> <b>#self</b>{ <span style='color:#b08000;'>0</span> }
  }
}</pre>

A category can also have `@category` members, but *not* `@type` members. (The
latter is so that the runtime implementation can clean up unused `@type`s
without introducing ambiguitites regarding member lifespan.)

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<b>concrete</b> <b><span style='color:#0057ae;'>MyCategory</span></b> {
  <span style='color:#644a9b;'>@type</span> global () -&gt; (<span style='color:#0057ae;'>MyCategory</span>)
}

<b>define</b> <b><span style='color:#0057ae;'>MyCategory</span></b> {
  <span style='color:#644a9b;'>@value</span> <i><span style='color:#0057ae;'>Int</span></i> value

  <span style='color:#898887;'>// @category members use inline initialization.</span>
  <span style='color:#644a9b;'>@category</span> <span style='color:#0057ae;'>MyCategory</span> singleton &lt;- <span style='color:#0057ae;'>MyCategory</span>{ <span style='color:#b08000;'>0</span> }

  global () {
    <span style='color:#898887;'>// @category members are accessible from all functions in the category.</span>
    <b>return</b> singleton
  }
}</pre>

#### Conditionals

Zeolite uses the `if`/`elif`/`else` conditional construct. The `elif` and `else`
clauses are always optional.

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<b>if</b> (x) {
  <span style='color:#898887;'>// something</span>
} <b>elif</b> (y) {
  <span style='color:#898887;'>// something</span>
} <b>else</b> {
  <span style='color:#898887;'>// something</span>
}</pre>

#### Scoping and Cleanup

Variables can be scoped to specific blocks of code. Additionally, you can
provide a cleanup procedure to be executed upon exit from the block of code.
This is useful if you want to free resources without needing to explicitly do so
for every `return` statement.

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<span style='color:#898887;'>// Simple scoping during evaluation.</span>
<b>scoped</b> {
  <i><span style='color:#0057ae;'>Int</span></i> x &lt;- getValue()
} <b>in</b> <b>if</b> (x &lt; <span style='color:#b08000;'>0</span>) {
  <span style='color:#898887;'>// ...</span>
} <b>elif</b> (x &gt; <span style='color:#b08000;'>0</span>) {
  <span style='color:#898887;'>// ...</span>
} <b>else</b> {
  <span style='color:#898887;'>// ...</span>
}

<span style='color:#898887;'>// Simple scoping during assignment.</span>
<b>scoped</b> {
  <i><span style='color:#0057ae;'>Int</span></i> x &lt;- getValue1()
  <i><span style='color:#0057ae;'>Int</span></i> y &lt;- getValue2()
} <b>in</b> <i><span style='color:#0057ae;'>Int</span></i> z &lt;- x+y

<span style='color:#898887;'>// Scoping with cleanup.</span>
<b>scoped</b> {
  <span style='color:#898887;'>// ...</span>
} <b>cleanup</b> {
  <span style='color:#898887;'>// ...</span>
} <b>in</b> {
  <span style='color:#898887;'>// ...</span>
}

<span style='color:#898887;'>// Cleanup without scoping.</span>
<b>cleanup</b> {
  i &lt;- i+<span style='color:#b08000;'>1</span>  <span style='color:#898887;'>// Post-increment behavior.</span>
} <b>in</b> <b>return</b> i</pre>

The `cleanup` block is executed at every `return`, `break`, and `continue` in
the respective `in` block, and right after the `in` block. For this reason, you
*cannot* use `return`, `break`, or `continue` within a `cleanup` block.
Additionally, you cannot overwrite named returns. You *can* use `fail`, however,
since that just ends program execution.

When `cleanup` is executed at a `return` statement in the `in` block, the
returns from the `return` statement are "locked in", then `cleanup` is executed,
*then* those locked-in return values are returned. (This is what allows the
post-increment example above to work.)

#### Loops

Zeolite supports two loop types:

1. **`while`** loops, which are the traditional repetition of a procedure while
   a predicate holds.

   <pre style='color:#1f1c1b;background-color:#f6f8fa;'>
   <span style='color:#898887;'>// With break and continue.</span>
   <b>while</b> (<b>true</b>) {
     <b>if</b> (<b>true</b>) {
       <b>break</b>
     } <b>else</b> {
       <b>continue</b>
     }
   }

   <span style='color:#898887;'>// With an update after each iteration.</span>
   <b>while</b> (<b>true</b>) {
     <span style='color:#898887;'>// ...</span>
   } <b>update</b> {
     <span style='color:#898887;'>// ...</span>
   }</pre>

2. **`traverse`** loops (as of compiler version `0.16.0.0`), which automatically
   iterate over the `#x` values in an `optional Order<#x>`. This is similar to
   `for (int i : container) { ... }` in C++ and `for i in container: ...` in
   Python.

   <pre style='color:#1f1c1b;background-color:#f6f8fa;'>
   <b>traverse</b> (orderedStrings -&gt; <i><span style='color:#0057ae;'>String</span></i> s) {
     <span style='color:#898887;'>// executed once per String s in orderedStrings</span>
     <span style='color:#898887;'>// you can also use break and continue</span>
   }

   <span style='color:#898887;'>// With an update after each iteration.</span>
   <b>traverse</b> (orderedStrings -&gt; <i><span style='color:#0057ae;'>String</span></i> s) {
     <span style='color:#898887;'>// ...</span>
   } <b>update</b> {
     <span style='color:#898887;'>// ...</span>
   }</pre>

   Since the `Order` is optional, `empty` can be used to iterate zero times.

   **IMPORTANT:** Most containers *are not* iterable by `traverse` as-is; you
   will need to call a `@value` function to get the `Order`. Some categories
   `refine DefaultOrder<#x>` (such as `String`, and `Vector` in
   `lib/container`), which allows you to use its `defaultOrder()`. Other
   categories provide multiple ways to `Order` the container, such as
   `SearchTree` in `lib/container`.

   <pre style='color:#1f1c1b;background-color:#f6f8fa;'>
   <b>traverse</b> (<span style='color:#bf0303;'>&quot;hello&quot;</span><span style='color:#644a9b;'>.</span>defaultOrder() -&gt; <i><span style='color:#0057ae;'>Char</span></i> c) {
     <span style='color:#898887;'>// executed once per Char c in &quot;hello&quot;</span>
   }</pre>

`for` loops (e.g., `for (int i = 0; i < foo; ++i) { ... }` in C++) *are not*
supported, since such syntax is too restrictive to scale, and they can be
replaced with `traverse` or `scoped`+`while` in nearly all situations.

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<span style='color:#898887;'>// Combine while with scoped to create a for loop.</span>
<b>scoped</b> {
  <i><span style='color:#0057ae;'>Int</span></i> i &lt;- <span style='color:#b08000;'>0</span>
  <i><span style='color:#0057ae;'>Int</span></i> limit &lt;- <span style='color:#b08000;'>10</span>
} <b>in</b> <b>while</b> (i &lt; limit) {
  <span style='color:#898887;'>// ...</span>
} <b>update</b> {
  i &lt;- i+<span style='color:#b08000;'>1</span>
}</pre>

#### Multiple Returns

A procedure definition has two options for returning multiple values:

1. Return all values. (Prior to compiler version `0.3.0.0`, multiple returns
were enclosed in  `{}`, e.g., `return { x, y }`.)

   <pre style='color:#1f1c1b;background-color:#f6f8fa;'>
   <b>define</b> <b><span style='color:#0057ae;'>MyCategory</span></b> {
     minMax (x,y) {
       <b>if</b> (x &lt; y) {
         <b>return</b> x, y
       } <b>else</b> {
         <b>return</b> y, x
       }
     }
   }</pre>

2. Naming the return values and assigning them individually. This can be useful
   (and less error-prone) if the values are determined at different times.
   The compiler uses static analysis to ensure that all named variables are
   guaranteed to be set via all possible control paths.

   <pre style='color:#1f1c1b;background-color:#f6f8fa;'>
   <b>define</b> <b><span style='color:#0057ae;'>MyCategory</span></b> {
     <span style='color:#898887;'>// Returns are named on the first line.</span>
     minMax (x,y) (min,max) {
       <span style='color:#898887;'>// Returns are optionally initialized up front.</span>
       min &lt;- y
       max &lt;- x
       <b>if</b> (x &lt; y) {
         <span style='color:#898887;'>// Returns are overwritten.</span>
         min &lt;- x
         max &lt;- y
       }
       <span style='color:#898887;'>// Implicit return makes sure that all returns are assigned. Optionally,</span>
       <span style='color:#898887;'>// you can use return _.</span>
     }
   }</pre>

The caller of a function with multiple returns also has a few options:

1. Assign the returns to a set of variables. You can ignore a position by using
   `_` in that position. (Prior to compiler version `0.3.0.0`, multiple
   assignments were enclosed in `{}`, e.g., `{ Int min, _ } <- minMax(4,3)`.)

  <pre style='color:#1f1c1b;background-color:#f6f8fa;'>
  <i><span style='color:#0057ae;'>Int</span></i> min, <b>_</b> &lt;- minMax(<span style='color:#b08000;'>4</span>,<span style='color:#b08000;'>3</span>)</pre>

2. Pass them directly to a function that requires the same number of
   compatible arguments. (Note that you *cannot* concatenate the returns of
   multiple functions.)

  <pre style='color:#1f1c1b;background-color:#f6f8fa;'>
  <i><span style='color:#0057ae;'>Int</span></i> delta &lt;- diff(minMax(<span style='color:#b08000;'>4</span>,<span style='color:#b08000;'>3</span>))</pre>

#### Optional and Weak Values

Zeolite requires that all variables be initialized; however, it provides the
**`optional`** storage modifier to allow a *specific* variable to be
**`empty`**. This *is not* the same as `null` in Java because `optional`
variables need to be `require`d before use.

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<span style='color:#898887;'>// empty is a special value for use with optional.</span>
<b>optional</b> <i><span style='color:#0057ae;'>Int</span></i> value &lt;- <b>empty</b>

<span style='color:#898887;'>// Non-optional values automatically convert to optional.</span>
value &lt;- <span style='color:#b08000;'>1</span>

<span style='color:#898887;'>// present returns true iff the value is not empty.</span>
<b>if</b> (<b>present</b>(value)) {
  <span style='color:#898887;'>// Use require to convert the value to something usable.</span>
  <span style='color:#006e28;'>\</span> foo(<b>require</b>(value))
}</pre>

**`weak`** values allow your program to access a value *if it is available*,
without holding up that value's cleanup if nothing else needs it. This can be
used to let threads clean themselves up (example below) or to handle cycles in
references between objects.

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<b>concrete</b> <b><span style='color:#0057ae;'>MyRoutine</span></b> {
  <span style='color:#644a9b;'>@type</span> createAndRun () -&gt; (<span style='color:#0057ae;'>MyRoutine</span>)
  <span style='color:#644a9b;'>@value</span> waitCompletion () -&gt; ()
}

<b>define</b> <b><span style='color:#0057ae;'>MyRoutine</span></b> {
  <b>refines</b> <span style='color:#0057ae;'>Routine</span>

  <span style='color:#898887;'>// (See lib/thread.)</span>
  <span style='color:#644a9b;'>@value</span> <b>weak</b> <span style='color:#0057ae;'>Thread</span> thread

  createAndRun () {
    <span style='color:#898887;'>// Create a new MyRoutine and then start the thread.</span>
    <b>return</b> <span style='color:#0057ae;'>MyRoutine</span>{ <b>empty</b> }<span style='color:#644a9b;'>.</span>start()
  }

  run () {
    <span style='color:#898887;'>// routine</span>
  }

  waitCompletion () {
    <b>scoped</b> {
      <span style='color:#898887;'>// Use strong to turn weak into optional. If the return is non-empty, the</span>
      <span style='color:#898887;'>// value is guaranteed to remain valid while using thread2.</span>
      <b>optional</b> <span style='color:#0057ae;'>Thread</span> thread2 &lt;- <b>strong</b>(thread)
    } <b>in</b> <b>if</b> (<b>present</b>(thread2)) {
      <span style='color:#006e28;'>\</span> <b>require</b>(thread2)<span style='color:#644a9b;'>.</span>join()
    }
  }

  <span style='color:#644a9b;'>@value</span> start () -&gt; (<b>#self</b>)
  start () {
    <span style='color:#898887;'>// ProcessThread holds a reference to itself only while the Routine is</span>
    <span style='color:#898887;'>// running. Making thread weak means that the ProcessThread can clean itself</span>
    <span style='color:#898887;'>// up once the Routine terminates.</span>
    thread &lt;- <span style='color:#0057ae;'>ProcessThread</span><span style='color:#644a9b;'>.</span>from(<b>self</b>)<span style='color:#644a9b;'>.</span>start()
    <b>return</b> <b>self</b>
  }
}</pre>

#### Deferred Variable Initialization

In some situations, a variable's value depends on conditional logic, and there
is no low-cost default value. In such situations, you can use the **`defer`**
keyword to allow a variable to be *temporarily* uninitialized. (As of compiler
version `0.20.0.0`.)

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<span style='color:#0057ae;'>LargeObject</span> object &lt;- <b>defer</b>

<b>if</b> (debug) {
  object &lt;- <span style='color:#0057ae;'>LargeObject</span><span style='color:#644a9b;'>.</span>newDebug()
} <b>else</b> {
  object &lt;- <span style='color:#0057ae;'>LargeObject</span><span style='color:#644a9b;'>.</span>new()
}

<span style='color:#006e28;'>\</span> object<span style='color:#644a9b;'>.</span>execute()</pre>

In this example, `object` is declared without an initializer, and is then
initialized in *both* the `if` and `else` clauses.

- A variable initialized with `defer` *must* be initialized via *all possible*
  control paths prior to its use. This is checked at compile time.
- An *existing* variable can also be marked as `defer`red. This *will not*
  change its value, but will instead require that it be assigned a new value
  before it gets used again.
- If you never *read* the variable in a particular control branch then you do
  not need to initialize it; initialization is only checked where necessary.

### Using Parameters

All `concrete` categories and all `interface`s can have type parameters. Each
parameter can have a variance rule assigned to it. This allows the compiler to
do type conversions between different parameterizations.

Parameter names must start with `#` and a lowercase letter, and can only contain
letters and digits.

Parameters are *never* repeated in the category or function definitions. (Doing
so would just create more opportunity for unnecessary compile-time errors.)

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<span style='color:#898887;'>// #x is covariant (indicated by being to the right of |), which means that it</span>
<span style='color:#898887;'>// can only be used for output purposes.</span>
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Reader</span></b><span style='color:#c02040;'>&lt;</span><span style='color:#c04040;'>|</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  read () -&gt; (<i><span style='color:#0057ae;'>#x</span></i>)
}

<span style='color:#898887;'>// #x is contravariant (indicated by being to the left of |), which means that</span>
<span style='color:#898887;'>// it can only be used for input purposes.</span>
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Writer</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c04040;'>|</span><span style='color:#c02040;'>&gt;</span> {
  write (<i><span style='color:#0057ae;'>#x</span></i>) -&gt; ()
}

<span style='color:#898887;'>// #x is for output and #y is for input, from the caller's perspective.</span>
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Function</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c04040;'>|</span><i><span style='color:#0057ae;'>#y</span></i><span style='color:#c02040;'>&gt;</span> {
  call (<i><span style='color:#0057ae;'>#x</span></i>) -&gt; (<i><span style='color:#0057ae;'>#y</span></i>)
}

<span style='color:#898887;'>// By default, parameters are invariant, i.e., cannot be converted. You can also</span>
<span style='color:#898887;'>// explicitly specify invariance with &lt;|#x|&gt;. This allows all three variance</span>
<span style='color:#898887;'>// types to be present.</span>
<b>concrete</b> <b><span style='color:#0057ae;'>List</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  <span style='color:#644a9b;'>@value</span> append (<i><span style='color:#0057ae;'>#x</span></i>) -&gt; ()
  <span style='color:#644a9b;'>@value</span> head () -&gt; (<i><span style='color:#0057ae;'>#x</span></i>)
}</pre>

- Specifying parameter variance allows the compiler to automatically convert
  between different types. This is done recursively in terms of parameter
  substitution.

  <pre style='color:#1f1c1b;background-color:#f6f8fa;'>
  <span style='color:#898887;'>// Covariance allows conversion upward.</span>
  <span style='color:#0057ae;'>Reader</span><span style='color:#c02040;'>&lt;</span><span style='color:#0057ae;'>MyValue</span><span style='color:#c02040;'>&gt;</span> reader &lt;- <span style='color:#898887;'>// ...</span>
  <span style='color:#0057ae;'>Reader</span><span style='color:#c02040;'>&lt;</span><span style='color:#0057ae;'>MyBase</span><span style='color:#c02040;'>&gt;</span>  reader2 &lt;- reader

  <span style='color:#898887;'>// Contravariance allows conversion downward.</span>
  <span style='color:#0057ae;'>Writer</span><span style='color:#c02040;'>&lt;</span><span style='color:#0057ae;'>MyBase</span><span style='color:#c02040;'>&gt;</span>  writer &lt;- <span style='color:#898887;'>// ...</span>
  <span style='color:#0057ae;'>Writer</span><span style='color:#c02040;'>&lt;</span><span style='color:#0057ae;'>MyValue</span><span style='color:#c02040;'>&gt;</span> writer2 &lt;- writer

  <span style='color:#898887;'>// Conversion is also recursive.</span>
  <span style='color:#0057ae;'>Writer</span><span style='color:#c02040;'>&lt;</span><span style='color:#0057ae;'>Reader</span><span style='color:#c02040;'>&lt;</span><span style='color:#0057ae;'>MyBase</span><span style='color:#c02040;'>&gt;&gt;</span>  readerWriter &lt;- <span style='color:#898887;'>// ...</span>
  <span style='color:#0057ae;'>Writer</span><span style='color:#c02040;'>&lt;</span><span style='color:#0057ae;'>Reader</span><span style='color:#c02040;'>&lt;</span><span style='color:#0057ae;'>MyValue</span><span style='color:#c02040;'>&gt;&gt;</span> readerWriter2 &lt;- readerWriter

  <span style='color:#898887;'>// Invariance does not allow conversions.</span>
  <span style='color:#0057ae;'>List</span><span style='color:#c02040;'>&lt;</span><span style='color:#0057ae;'>MyValue</span><span style='color:#c02040;'>&gt;</span> list &lt;- <span style='color:#898887;'>// ...</span>
  <span style='color:#0057ae;'>List</span><span style='color:#c02040;'>&lt;</span><span style='color:#0057ae;'>MyBase</span><span style='color:#c02040;'>&gt;</span>  list2 &lt;- <span style='color:#898887;'>// ...</span></pre>

- You can apply **filters** to type parameters to require that the parameters
  meet certain requirements. This allows you to call `interface` functions on
  parameters and their values in procedure definitions.

  <pre style='color:#1f1c1b;background-color:#f6f8fa;'>
  <b>concrete</b> <b><span style='color:#0057ae;'>Box</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
    <span style='color:#898887;'>// #x must implement the Formatted builtin @value interface.</span>
    <i><span style='color:#0057ae;'>#x</span></i> <b>requires</b> <i><span style='color:#0057ae;'>Formatted</span></i>

    <span style='color:#644a9b;'>@type</span> asString (<i><span style='color:#0057ae;'>#x</span></i>) -&gt; (<i><span style='color:#0057ae;'>String</span></i>)
  }

  <b>define</b> <b><span style='color:#0057ae;'>Box</span></b> {
    asString (x) {
      <span style='color:#898887;'>// The &quot;requires&quot; filter allows you to call Formatted functions.</span>
      <b>return</b> x<span style='color:#644a9b;'>.</span>formatted()
    }
  }</pre>

  **IMPORTANT:** As of compiler version `0.16.0.0`, you can no longer use
  parameter filters in `@value interface`s and `@type interface`s.

- You can call `@type` functions on parameters as if they were regular types.
  You can only call functions that are implied by a `defines` filter.

  <pre style='color:#1f1c1b;background-color:#f6f8fa;'>
  <b>concrete</b> <b><span style='color:#0057ae;'>MyCategory</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
    <i><span style='color:#0057ae;'>#x</span></i> <b>defines</b> <i><span style='color:#0057ae;'>LessThan</span></i><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span>
    <span style='color:#644a9b;'>@type</span> compare (<i><span style='color:#0057ae;'>#x</span></i>,<i><span style='color:#0057ae;'>#x</span></i>) -&gt; (<i><span style='color:#0057ae;'>Int</span></i>)
  }

  <b>define</b> <b><span style='color:#0057ae;'>MyCategory</span></b> {
    compare (x,y) {
      <b>if</b> (<i><span style='color:#0057ae;'>#x</span></i><span style='color:#644a9b;'>.</span>lessThan(x,y)) {
        <b>return</b> -<span style='color:#b08000;'>1</span>
      } <b>elif</b> (<i><span style='color:#0057ae;'>#x</span></i><span style='color:#644a9b;'>.</span>lessThan(y,x)) {
        <b>return</b> <span style='color:#b08000;'>1</span>
      } <b>else</b> {
        <b>return</b> <span style='color:#b08000;'>0</span>
      }
    }
  }

  <span style='color:#898887;'>// ...</span>

  <i><span style='color:#0057ae;'>Int</span></i> comp &lt;- <span style='color:#0057ae;'>MyCategory</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>String</span></i><span style='color:#c02040;'>&gt;</span><span style='color:#644a9b;'>.</span>compare(<span style='color:#bf0303;'>&quot;x&quot;</span>,<span style='color:#bf0303;'>&quot;y&quot;</span>)</pre>

- All of the above is also possible with **function parameters**, aside from
  specifying parameter variance.

  <pre style='color:#1f1c1b;background-color:#f6f8fa;'>
  <b>concrete</b> <b><span style='color:#0057ae;'>MyCategory</span></b> {
    <span style='color:#644a9b;'>@type</span> compare&lt;<i><span style='color:#0057ae;'>#x</span></i>&gt;
      <i><span style='color:#0057ae;'>#x</span></i> <b>defines</b> <i><span style='color:#0057ae;'>LessThan</span></i><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span>
    (<i><span style='color:#0057ae;'>#x</span></i>,<i><span style='color:#0057ae;'>#x</span></i>) -&gt; (<i><span style='color:#0057ae;'>Int</span></i>)
  }

  <b>define</b> <b><span style='color:#0057ae;'>MyCategory</span></b> {
    compare (x,y) {
      <b>if</b> (<i><span style='color:#0057ae;'>#x</span></i><span style='color:#644a9b;'>.</span>lessThan(x,y)) {
        <b>return</b> -<span style='color:#b08000;'>1</span>
      } <b>elif</b> (<i><span style='color:#0057ae;'>#x</span></i><span style='color:#644a9b;'>.</span>lessThan(y,x)) {
        <b>return</b> <span style='color:#b08000;'>1</span>
      } <b>else</b> {
        <b>return</b> <span style='color:#b08000;'>0</span>
      }
    }
  }

  <span style='color:#898887;'>// ...</span>

  <i><span style='color:#0057ae;'>Int</span></i> comp &lt;- <span style='color:#0057ae;'>MyCategory</span><span style='color:#644a9b;'>.</span>compare&lt;<i><span style='color:#0057ae;'>String</span></i>&gt;(<span style='color:#bf0303;'>&quot;x&quot;</span>,<span style='color:#bf0303;'>&quot;y&quot;</span>)</pre>

### Using Interfaces

Zeolite has `@value interface`s that are similar to Java `interface`s, which
declare functions that implementations must define. In addition, Zeolite also
has `@type interface`s that declare `@type` functions that must be defined.
(This would be like having `abstract static` functions in Java.)

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<span style='color:#898887;'>// @value indicates that the interface declares @value functions.</span>
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Printable</span></b> {
  <span style='color:#898887;'>// @value is not allowed in the declaration.</span>
  print () -&gt; ()
}

<span style='color:#898887;'>// @type indicates that the interface declares @type functions.</span>
<span style='color:#644a9b;'>@type</span> <b>interface</b> <b><span style='color:#0057ae;'>Diffable</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  <span style='color:#898887;'>// @type is not allowed in the declaration.</span>
  diff (<i><span style='color:#0057ae;'>#x</span></i>,<i><span style='color:#0057ae;'>#x</span></i>) -&gt; (<i><span style='color:#0057ae;'>#x</span></i>)
}</pre>

- `@value interface`s can be **inherited** by other `@value interface`s and
  `concrete` categories using `refines`.

  <pre style='color:#1f1c1b;background-color:#f6f8fa;'>
  <b>concrete</b> <b><span style='color:#0057ae;'>MyValue</span></b> {
    <b>refines</b> <span style='color:#0057ae;'>Printable</span>

    <span style='color:#898887;'>// The functions of Printable do not need to be declared again, but you can do</span>
    <span style='color:#898887;'>// so to refine the argument and return types.</span>
  }</pre>

- `@type interface`s can only be **inherited** by `concrete` categories.

  <pre style='color:#1f1c1b;background-color:#f6f8fa;'>
  <b>concrete</b> <b><span style='color:#0057ae;'>MyValue</span></b> {
    <b>defines</b> <span style='color:#0057ae;'>Diffable</span><span style='color:#c02040;'>&lt;</span><span style='color:#0057ae;'>MyValue</span><span style='color:#c02040;'>&gt;</span>

    <span style='color:#898887;'>// The functions of Diffable do not need to be declared again, but you can do</span>
    <span style='color:#898887;'>// so to refine the argument and return types.</span>
  }</pre>

- You can also specify `refines` and `defines` when *defining* a `concrete`
  category. This allows the inheritance to be private.

  <pre style='color:#1f1c1b;background-color:#f6f8fa;'>
  <b>concrete</b> <b><span style='color:#0057ae;'>MyValue</span></b> {
    <span style='color:#644a9b;'>@type</span> create () -&gt; (<i><span style='color:#0057ae;'>Formatted</span></i>)
  }

  <b>define</b> <b><span style='color:#0057ae;'>MyValue</span></b> {
    <span style='color:#898887;'>// Formatted is not a visible parent outside of MyValue.</span>
    <b>refines</b> <i><span style='color:#0057ae;'>Formatted</span></i>

    create () {
      <b>return</b> <span style='color:#0057ae;'>MyValue</span>{ }
    }

    <span style='color:#898887;'>// Inherited from Formatted.</span>
    formatted () {
      <b>return</b> <span style='color:#bf0303;'>&quot;MyValue&quot;</span>
    }
  }</pre>

### Immutable Types

You can modify `interface` and `concrete` with **`immutable`** at the very top
of the declaration. (As of compiler version `0.20.0.0`.) This creates two
requirements for `@value` members:

1. They are marked as read-only, and cannot be overwritten with `<-`.
2. They must have a type that is also `immutable`.

(`@category` members are not affected.)

Note that this applies to the *entire* implementation; not just to the
implementations of functions required by the `immutable` `interface`.
`immutable` is therefore intended for objects that cannot be modified, rather
than as a way to define a read-only view (e.g., `const` in C++) of an object.

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Foo</span></b> {
  <b>immutable</b>

  call () -&gt; ()
}

<b>concrete</b> <b><span style='color:#0057ae;'>Bar</span></b> {
  <b>refines</b> <span style='color:#0057ae;'>Foo</span>

  <span style='color:#644a9b;'>@type</span> new () -&gt; (<span style='color:#0057ae;'>Bar</span>)
  <span style='color:#644a9b;'>@value</span> mutate () -&gt; ()
}

<b>define</b> <b><span style='color:#0057ae;'>Bar</span></b> {
  <span style='color:#644a9b;'>@value</span> <i><span style='color:#0057ae;'>Int</span></i> value

  new () { <b>return</b> <span style='color:#0057ae;'>Bar</span>{ <span style='color:#b08000;'>0</span> } }

  call () {
    <span style='color:#898887;'>// call cannot overwrite value</span>
  }

  mutate () {
    <span style='color:#898887;'>// mutate also cannot overwrite value, even though mutate isn't in Foo.</span>
  }
}</pre>

For members that use a parameter as a type, you can use `immutable` as a filter
if the other filters do not otherwise imply it. Note that this will prevent
substituting in a non-`immutable` type when calling `@type` functions.

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<b>concrete</b> <b><span style='color:#0057ae;'>Type</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  <b>immutable</b>

  <i><span style='color:#0057ae;'>#x</span></i> <b>immutable</b>
}

<b>define</b> <b><span style='color:#0057ae;'>Type</span></b> {
  <span style='color:#898887;'>// #x is allowed as a member type because of the immutable filter.</span>
  <span style='color:#644a9b;'>@value</span> <i><span style='color:#0057ae;'>#x</span></i> value
}</pre>

### The `#self` Parameter

Every category has an implicit *covariant* parameter **`#self`**. (As of
compiler version `0.14.0.0`.) It *always* means the type of the current
category, even when inherited. (`#self` is covariant because it needs to be
convertible to a parent of the current category.)

For example:

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Iterator</span></b><span style='color:#c02040;'>&lt;</span><span style='color:#c04040;'>|</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  next () -&gt; (<b>#self</b>)
  get () -&gt; (<i><span style='color:#0057ae;'>#x</span></i>)
}

<b>concrete</b> <b><span style='color:#0057ae;'>CharIterator</span></b> {
  <b>refines</b> <span style='color:#0057ae;'>Iterator</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>Char</span></i><span style='color:#c02040;'>&gt;</span>
  <span style='color:#898887;'>// next must return CharIterator because #self = CharIterator here.</span>
}</pre>

The primary purpose of this is to support combining multiple interfaces with
iterator or builder semantics into composite types *without* getting backed into
a corner when calling functions from a single interface.

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>ForwardIterator</span></b><span style='color:#c02040;'>&lt;</span><span style='color:#c04040;'>|</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  next () -&gt; (<b>#self</b>)
  get () -&gt; (<i><span style='color:#0057ae;'>#x</span></i>)
}

<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>ReverseIterator</span></b><span style='color:#c02040;'>&lt;</span><span style='color:#c04040;'>|</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  prev () -&gt; (<b>#self</b>)
  get () -&gt; (<i><span style='color:#0057ae;'>#x</span></i>)
}

<b>concrete</b> <b><span style='color:#0057ae;'>CharIterator</span></b> {
  <b>refines</b> <span style='color:#0057ae;'>ForwardIterator</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>Char</span></i><span style='color:#c02040;'>&gt;</span>
  <b>refines</b> <span style='color:#0057ae;'>ReverseIterator</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>Char</span></i><span style='color:#c02040;'>&gt;</span>
  get () -&gt; (<i><span style='color:#0057ae;'>Char</span></i>)  <span style='color:#898887;'>// (Remember that merging needs to be done explicitly.)</span>
}

<b>concrete</b> <b><span style='color:#0057ae;'>Parser</span></b> {
  <span style='color:#898887;'>// trimWhitespace can call next and still return the original type. In</span>
  <span style='color:#898887;'>// contrast, if next returned ForwardIterator&lt;#x&gt; then trimWhitespace would</span>
  <span style='color:#898887;'>// need to return ForwardIterator&lt;Char&gt; to the caller instead of #i.</span>
  <span style='color:#644a9b;'>@type</span> trimWhitespace&lt;<i><span style='color:#0057ae;'>#i</span></i>&gt;
    <i><span style='color:#0057ae;'>#i</span></i> <b>requires</b> <span style='color:#0057ae;'>ForwardIterator</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>Char</span></i><span style='color:#c02040;'>&gt;</span>
  (<i><span style='color:#0057ae;'>#i</span></i>) -&gt; (<i><span style='color:#0057ae;'>#i</span></i>)
}</pre>

`#self` can also be used to generalize a factory pattern:

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<span style='color:#644a9b;'>@type</span> <b>interface</b> <b><span style='color:#0057ae;'>ParseFactory</span></b> {
  fromString (<i><span style='color:#0057ae;'>String</span></i>) -&gt; (<b>#self</b>)
}

<b>concrete</b> <b><span style='color:#0057ae;'>FileParser</span></b> {
  <span style='color:#644a9b;'>@type</span> parseFromFile&lt;<i><span style='color:#0057ae;'>#x</span></i>&gt;
    <i><span style='color:#0057ae;'>#x</span></i> <b>defines</b> <span style='color:#0057ae;'>ParseFactory</span>
  (<i><span style='color:#0057ae;'>String</span></i>) -&gt; (<i><span style='color:#0057ae;'>#x</span></i>)
}

<b>define</b> <b><span style='color:#0057ae;'>FileParser</span></b> {
  parseFromFile (filename) {
    <i><span style='color:#0057ae;'>String</span></i> content &lt;- <span style='color:#0057ae;'>FileHelper</span><span style='color:#644a9b;'>.</span>readAll(filename)
    <span style='color:#898887;'>// Notice that ParseFactory doesn't need a type parameter to indicate what</span>
    <span style='color:#898887;'>// type is going to be parsed in fromString; it's sufficient to know that #x</span>
    <span style='color:#898887;'>// implements ParseFactory and that fromString returns #self.</span>
    <b>return</b> <i><span style='color:#0057ae;'>#x</span></i><span style='color:#644a9b;'>.</span>fromString(content)
  }
}

<b>concrete</b> <b><span style='color:#0057ae;'>Value</span></b> {
  <b>defines</b> <span style='color:#0057ae;'>ParseFactory</span>
}

<b>define</b> <b><span style='color:#0057ae;'>Value</span></b> {
  fromString (string) {
    <b>if</b> (string == <span style='color:#bf0303;'>&quot;Value&quot;</span>) {
      <b>return</b> <span style='color:#0057ae;'>Value</span>{ }
    } <b>else</b> {
      <b>fail</b>(<span style='color:#bf0303;'>&quot;could not parse input&quot;</span>)
    }
  }
}</pre>

`#self` is nothing magical; this could all be done by explicitly adding a
covariant `#self` parameter to *every* type, with the appropriate `requires` and
`defines` filters.

### Type Inference

Starting with compiler version `0.7.0.0`, Zeolite supports optional inference of
specific function parameters by using **`?`**. This must be at the top level (no
nesting), and it cannot be used outside of the parameters of the function.

The type-inference system is intentionally "just clever enough" to do things
that the programmer can easily guess. More sophisticated inference is feasible
in theory (like Haskell uses); however, type errors with such systems can draw
a significant amount of attention away from the task at hand. (For example, a
common issue with Haskell is not knowing *which line of code* contains the
actual mistake causing a type error.)

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<b>concrete</b> <b><span style='color:#0057ae;'>Value</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  <span style='color:#644a9b;'>@category</span> create1&lt;<i><span style='color:#0057ae;'>#x</span></i>&gt; (<i><span style='color:#0057ae;'>#x</span></i>) -&gt; (<span style='color:#0057ae;'>Value</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span>)
  <span style='color:#644a9b;'>@type</span>     create2     (<i><span style='color:#0057ae;'>#x</span></i>) -&gt; (<span style='color:#0057ae;'>Value</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span>)
}

<span style='color:#898887;'>// ...</span>

<span style='color:#898887;'>// This is fine.</span>
<span style='color:#0057ae;'>Value</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>Int</span></i><span style='color:#c02040;'>&gt;</span> value1 &lt;- <span style='color:#0057ae;'>Value</span><span style='color:#644a9b;'>:</span>create1&lt;<b>?</b>&gt;(<span style='color:#b08000;'>10</span>)

<span style='color:#898887;'>// These uses of ? are not allowed:</span>
<span style='color:#898887;'>// Value&lt;Int&gt; value2 &lt;- Value&lt;?&gt;.create2(10)</span>
<span style='color:#898887;'>// Value&lt;?&gt;   value2 &lt;- Value&lt;Int&gt;.create2(10)</span>
</pre>

Only the function arguments and the parameter filters are used to infer the type
substitution; return types are ignored. If inference fails, you will see a
compiler error and will need to explicitly write out the type.

Type inference will *only* succeed if:

1. There is a valid pattern match between the expected argument types and the
   types of the passed arguments.

2. There is *exactly one* type that matches best:

   - For params only used in *covariant* positions, the *lower bound* of the
     type is unambiguous.
   - For params only used in *contravariant* positions, the *upper bound* of the
     type is unambiguous.
   - For all other situations, the *upper and lower bounds* are unambiguous and
     *equal* to each other.

Type inference in the context of parameterized types is specifically disallowed
in order to limit the amount of code the reader needs to search to figure out
what types are being used. Forcing explicit specification of types for local
variables is more work for the programmer, but it makes the code easier to
reason about later on.

### Other Features

This section discusses language features that are less frequently used.

#### Meta Types

Zeolite provides two **meta types** that allow unnamed combinations of other
types.

- A value with an **intersection type** `[A&B]` can be assigned from something
  that is *both* `A` and `B`, and can be assigned to *either* an `A` or `B`.
  There is a special empty intersection named **`any`** that can be assigned
  from any value but cannot be assigned to any other type.

  Intersections can be useful for requiring multiple interfaces without creating
  a new category that refines all of those interfaces. An intersection
  `[Foo&Bar]` in Zeolite is semantically similar to the existential type
  `forall a. (Foo a, Bar a) => a` in Haskell and `? extends Foo & Bar` in Java,
  except that in Zeolite `[Foo&Bar]` can be used as a first-class type.

  <pre style='color:#1f1c1b;background-color:#f6f8fa;'>
  <span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Reader</span></b> {}

  <span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Writer</span></b> {}

  <b>concrete</b> <b><span style='color:#0057ae;'>Data</span></b> {
    <b>refines</b> <span style='color:#0057ae;'>Reader</span>
    <b>refines</b> <span style='color:#0057ae;'>Writer</span>
    <span style='color:#644a9b;'>@type</span> new () <b><span style='color:#006e28;'>-&gt;</span></b> (<span style='color:#0057ae;'>Data</span>)
  }

  <span style='color:#898887;'>// ...</span>

  <b><span style='color:#006e28;'>[</span></b><span style='color:#0057ae;'>Reader</span><span style='color:#006e28;'>&amp;</span><span style='color:#0057ae;'>Writer</span><b><span style='color:#006e28;'>]</span></b> val <b><span style='color:#006e28;'>&lt;-</span></b> <span style='color:#0057ae;'>Data</span><span style='color:#644a9b;'>.</span>new()
  <span style='color:#0057ae;'>Reader</span> val2 <b><span style='color:#006e28;'>&lt;-</span></b> val
  <span style='color:#0057ae;'>Writer</span> val3 <b><span style='color:#006e28;'>&lt;-</span></b> val</pre>

- A value with a **union type** `[A|B]` can be assigned from *either* `A` or
  `B`, but can only be assigned to something that *both* `A` and `B` can be
  assigned to. There is a special empty union named **`all`** that cannot ever
  be assigned a value but that can be assigned to everything. (`empty` is
  actually of type `optional all`.)

  Unions can be useful if you want to artificially limit what implementations of
  a particular `@value interface` are allowed by a function argument, e.g.,
  a specific set of "verified" implementations.

  <pre style='color:#1f1c1b;background-color:#f6f8fa;'>
  <span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Printable</span></b> {}

  <b>concrete</b> <b><span style='color:#0057ae;'>Newspaper</span></b> {
    <b>refines</b> <span style='color:#0057ae;'>Printable</span>
    <span style='color:#644a9b;'>@type</span> new () <b><span style='color:#006e28;'>-&gt;</span></b> (<span style='color:#0057ae;'>Newspaper</span>)
  }

  <b>concrete</b> <b><span style='color:#0057ae;'>Magazine</span></b> {
    <b>refines</b> <span style='color:#0057ae;'>Printable</span>
    <span style='color:#644a9b;'>@type</span> new () <b><span style='color:#006e28;'>-&gt;</span></b> (<span style='color:#0057ae;'>Magazine</span>)
  }

  <span style='color:#898887;'>// ...</span>

  <b><span style='color:#006e28;'>[</span></b><span style='color:#0057ae;'>Newspaper</span><span style='color:#006e28;'>|</span><span style='color:#0057ae;'>Magazine</span><b><span style='color:#006e28;'>]</span></b> val <b><span style='color:#006e28;'>&lt;-</span></b> <span style='color:#0057ae;'>Newspaper</span><span style='color:#644a9b;'>.</span>new()
  <span style='color:#0057ae;'>Printable</span> val2 <b><span style='color:#006e28;'>&lt;-</span></b> val</pre>

#### Runtime Type Reduction

The **`reduce`** builtin function enables very limited runtime reasoning about
type conversion.

- The call `reduce<Foo,Bar>(value)` will return `value` with type `optional Bar`
  iff `Foo` can be converted to `Bar`. Note that `value` must itself be
  convertible to `optional Foo`.
- When type params are used, the types that are assigned at the point of
  execution are checked. For example, the result of `reduce<#x,#y>(value)` will
  depend on the *specific* types assigned to `#x` and `#y` upon execution.

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>AnyObject</span></b> {
  getAs&lt;<i><span style='color:#0057ae;'>#y</span></i>&gt; () -&gt; (<b>optional</b> <i><span style='color:#0057ae;'>#y</span></i>)
}

<b>concrete</b> <b><span style='color:#0057ae;'>Object</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  <span style='color:#644a9b;'>@category</span> create&lt;<i><span style='color:#0057ae;'>#x</span></i>&gt; (<i><span style='color:#0057ae;'>#x</span></i>) -&gt; (<span style='color:#0057ae;'>AnyObject</span>)
}

<b>define</b> <b><span style='color:#0057ae;'>Object</span></b> {
  <b>refines</b> <span style='color:#0057ae;'>AnyObject</span>

  <span style='color:#644a9b;'>@value</span> <i><span style='color:#0057ae;'>#x</span></i> value

  create (value) { <b>return</b> <span style='color:#0057ae;'>Object</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span>{ value } }
  getAs  ()      { <b>return</b> <b>reduce</b>&lt;<i><span style='color:#0057ae;'>#x</span></i>,<i><span style='color:#0057ae;'>#y</span></i>&gt;(value) }
}</pre>

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<span style='color:#0057ae;'>AnyObject</span> value &lt;- <span style='color:#0057ae;'>Object</span><span style='color:#644a9b;'>:</span>create&lt;<b>?</b>&gt;(<span style='color:#bf0303;'>&quot;message&quot;</span>)

<span style='color:#898887;'>// This will be empty because String does not convert to Int.</span>
<b>optional</b> <i><span style='color:#0057ae;'>Int</span></i> value1 &lt;- value<span style='color:#644a9b;'>.</span>getAs&lt;<i><span style='color:#0057ae;'>Int</span></i>&gt;()

<span style='color:#898887;'>// This will be &quot;message&quot; as Formatted because String converts to Formatted.</span>
<b>optional</b> <i><span style='color:#0057ae;'>Formatted</span></i> value2 &lt;- value<span style='color:#644a9b;'>.</span>getAs&lt;<i><span style='color:#0057ae;'>Formatted</span></i>&gt;()</pre>

`reduce` *cannot* be used to "downcast" a value (e.g., converting a `Formatted`
to a `Float`) since the argument has the same type as the first parameter.

For example, `reduce<#x,#y>(value)` checks `#x`&rarr;`#y`, and since `value`
must be `optional #x`, `value` can only be converted upward. In other words, it
only allows conversions that would otherwise be allowed, returning `empty` for
all other conversions.

The `AnyObject` example above works because `Object` stores the *original type*
passed to `create` as `#x`, which it then has available for the `reduce` call.
The type variables `#x` and `#y` are the primary inputs to `reduce`; there is
absolutely no examination of the "real" type of `value` at runtime.

### Builtins

#### Reserved Words

[`#self`](#the-self-parameter)
[`@category`](#declaring-functions)
[`@type`](#declaring-functions)
[`@value`](#declaring-functions)
[`_`](#calling-functions)
[`all`](#meta-types)
[`allows`](#using-parameters)
[`any`](#meta-types)
[`break`](#loops)
[`cleanup`](#scoping-and-cleanup)
[`concrete`](#basic-ideas)
[`continue`](#loops)
[`defer`](#deferred-variable-initialization)
[`define`](#basic-ideas)
[`defines`](#using-interfaces)
[`elif`](#conditionals)
[`else`](#conditionals)
[`empty`](#optional-and-weak-values)
[`fail`](#calling-functions)
[`false`](#builtin-types)
[`if`](#conditionals)
[`immutable`](#immutable-types)
[`in`](#scoping-and-cleanup)
[`interface`](#using-interfaces)
[`optional`](#optional-and-weak-values)
[`present`](#optional-and-weak-values)
[`reduce`](#runtime-type-reduction)
[`refines`](#using-interfaces)
[`require`](#optional-and-weak-values)
[`requires`](#using-parameters)
[`return`](#defining-functions)
[`scoped`](#scoping-and-cleanup)
[`self`](#defining-functions)
[`strong`](#optional-and-weak-values)
[`testcase`](#writing-tests)
[`traverse`](#loops)
[`true`](#builtin-types)
[`typename`](#builtin-functions)
[`unittest`](#writing-tests)
[`update`](#loops)
[`weak`](#optional-and-weak-values)
[`while`](#loops)

#### Builtin Types

See
[`builtin.0rp`](https://github.com/ta0kira/zeolite/blob/master/base/builtin.0rp)
for more details about builtin types. (For your locally-installed version, which
might differ, see `$(zeolite --get-path)/base/builtin.0rp`.)

Builtin `concrete` types:

- **`Bool`**: Either `true` or `false`.
- **`Char`**: Use single quotes, e.g., `'a'`. Use literal characters, standard
  escapes (e.g., `'\n'`), 2-digit hex (e.g., `\x0B`), or 3-digit octal (e.g.,
  `'\012'`). At the moment this only supports ASCII; see
  [Issue #22](https://github.com/ta0kira/zeolite/issues/22).
- **`CharBuffer`**: Mutable, fixed-size buffer of `Char`. This type has no
  literals.
- **`Float`**: Use decimal notation, e.g., `0.0` or `1.0E1`. You *must* have
  digits on both sides of the `.`.
- **`Int`**: Use decimal (e.g., `1234`), hex (e.g., `\xABCD`), octal (e.g.,
  `\o0123`), or binary (e.g., `\b0100`).
- **`String`**: Use double quotes to sequence `Char` literals, e.g.,
  `"hello\012"`. You can build a string efficiently using `String.builder()`,
  e.g., `String foo <- String.builder().append("bar").append("baz").build()`.

Builtin `@value interface`s:

  built-in loop iteration.
- **`Append<#x>`**: Supports appending `#x`.
- **`AsBool`**: Convert a value to `Bool` using `asBool()`.
- **`AsChar`**: Convert a value to `Char` using `asChar()`.
- **`AsFloat`**: Convert a value to `Float` using `asFloat()`.
- **`AsInt`**: Convert a value to `Int` using `asInt()`.
- **`Build<#x>`**: Build a `#x` from the current state.
- **`Container`**: Contains multiple values.
- **`DefaultOrder<#x>`**: The container provides a default `Order<#x>` for use
  with `traverse`.
- **`Formatted`**: Format a value as a `String` using `formatted()`.
- **`Order<#x>`**: An ordering of `#x` values, for use with the `traverse`
- **`ReadAt<#x>`**: Random access reads from a container with values of type
  `#x`.
- **`SubSequence`**: Extract a subsequence from the object.
- **`WriteAt<#x>`**: Random access writes to a container with values of type
  `#x`.

Builtin `@type interface`s:

- **`Default`**: Get the default `@value` with `default()`.
- **`Equals<#x>`**: Compare values using `equals(x,y)`.
- **`LessThan<#x>`**: Compare values using `lessThan(x,y)`.

Builtin meta-types:

- **`#self`**: The type of the category where it is used. See
  [`#self`](#the-self-parameter).
- **`any`**: Value type that can be assigned a value of any type. (This is the
  [terminal object][initial-terminal] in the [category][category] of Zeolite
  types.)
- **`all`**: Value type that can be assigned to all other types. (This is the
  [initial object][initial-terminal] in the [category][category] of Zeolite
  types.)

#### Builtin Constants

- **`empty`** (`optional all`): A missing `optional` value.
- **`self`** (`#self`): The value being operated on in `@value` functions.

#### Builtin Functions

- **`present`**: Check `optional` for `empty`.
- **`reduce<#x,#y>(value)`**: See
  [Runtime Type Reduction](#runtime-type-reduction).
- **`require`**: Convert `optional` to non-`optional`.
- **`strong`**: Convert `weak` to `optional`.
- **`typename<#x>()`**: Formats the *real* type of `#x` as a `Formatted` value.

## Layout and Dependencies

### Using Public Source Files

You can create **public** `.0rp` source files to declare `concrete` categories
and `interface`s that are available for use in other sources. This is the only
way to share code between different source files. `.0rp` *cannot* contain
`define`s for `concrete` categories.

During compilation, *all* `.0rp` files in the project directory are loaded up
front. This is then used as the set of public symbols available when each `.0rx`
is *separately* compiled.

### Standard Library

The standard library currently temporary and lacks a lot of functionality. See
the public `.0rp` sources in [`lib`][lib]. Documentation will eventually follow.

### Modules

You can depend on another module using `-i lib/util` for a public dependency and
`-I lib/util` for a private dependency when calling `zeolite`. (A private
dependency is not  visible to modules that depend on your module.)

Dependency paths are first checked relative to the module depending on them. If
the dependency is not found there, the compiler then checks the global location
specified by `zeolite --get-path`.

Public `.0rp` source files are loaded from all dependencies during compilation,
and so their symbols are available to all source files in the module. There is
currently no language syntax for explicitly importing or including modules or
other symbols.

If you are interested in backing a `concrete` category with C++, you will need
to write a custom `.zeolite-module` file. Better documentation will eventually
follow, but for now:

1. Create a `.0rp` with declarations of all of the `concrete` categories you
   intend to define in C++ code.
2. Run `zeolite` in `--templates` mode to generate `.cpp` templates for all
   `concrete` categories that lack a definition in your module.
3. Run `zeolite` in `-c` mode to get a basic `.zeolite-module`. After this,
   *always* use recompile mode (`-r`) to use your `.zeolite-module`.
4. Take a look at `.zeolite-module` in [`lib/file`][lib/file] to get an idea of
   how to tell the compiler where your category definitions are.
5. Add your code to the generated `.cpp` files. [`lib/file`][lib/file] is also
   a reasonable example for this.
6. If you need to depend on external libraries, fill in the `include_paths` and
   `link_flags` sections of `.zeolite-module`.

**IMPORTANT:** `@value` functions for `immutable` categories will be marked as
`const` in C++ extensions. `immutable` also requires that `@value` members have
`immutable` types, but there is no reasonable way to enforce this in C++. You
will need to separately ensure that the implementation only stores other
`immutable` types, just for consistency with categories implemented in Zeolite.

## Unit Testing

Unit testing is a built-in capability of Zeolite. Unit tests use `.0rt` source
files, which are like `.0rx` source files with `testcase` metadata. The test
files go in the same directory as the rest of your source files. (Elsewhere in
this project these tests are referred to as "integration tests" because this
testing mode is used to ensure that the `zeolite` compiler operates properly
end-to-end.)

### Writing Tests

**IMPORTANT:** Prior to compiler version `0.10.0.0`, the `testcase` syntax was
slightly different, and `unittest` was not available.

<pre style='color:#1f1c1b;background-color:#f6f8fa;'>
<span style='color:#898887;'>// myprogram/tests.0rt</span>

<span style='color:#898887;'>// Each testcase starts with a header specifying a name for the group of tests.</span>
<span style='color:#898887;'>// This provides common setup code for a group of unit tests.</span>

<b><span style='color:#bf0303;background:#f7e6e6;'>testcase</span></b> <span style='color:#bf0303;'>&quot;passing tests&quot;</span> {
  <span style='color:#898887;'>// All unittest are expected to execute without any issues.</span>
  <span style='color:#04e040;'>success</span>
}

<span style='color:#898887;'>// Everything after the testcase (up until the next testcase) is like a .0rx.</span>

<span style='color:#898887;'>// At least one unittest must be defined when success is expected. Each unittest</span>
<span style='color:#898887;'>// must have a distinct name within the testcase. Each unittest is run in a</span>
<span style='color:#898887;'>// separate process, making it safe to alter global state.</span>

<b>unittest</b> <b><span style='color:#0057ae;'>myTest1</span></b> {
  <span style='color:#898887;'>// The test content goes here. It has access to anything within the testcase</span>
  <span style='color:#898887;'>// besides other unittest.</span>
}

<b>unittest</b> <b><span style='color:#0057ae;'>myTest2</span></b> {
  <span style='color:#006e28;'>\</span> <b>empty</b>
}


<span style='color:#898887;'>// A new testcase header indicates the end of the previous test.</span>

<b><span style='color:#bf0303;background:#f7e6e6;'>testcase</span></b> <span style='color:#bf0303;'>&quot;missing function&quot;</span> {
  <span style='color:#898887;'>// The test is expected to have a compilation error. Note that this cannot be</span>
  <span style='color:#898887;'>// used to check for parser failures!</span>
  <span style='color:#898887;'>//</span>
  <span style='color:#898887;'>// Any testcase can specify require and exclude regex patterns for checking</span>
  <span style='color:#898887;'>// test output. Each pattern can optionally be qualified with one of compiler,</span>
  <span style='color:#898887;'>// stderr, or stdout, to specify the source of the output.</span>
  <span style='color:#04e040;'>error</span>
  <span style='color:#04e040;'>require</span> <span style='color:#04e040;'>compiler</span> <span style='color:#bf0303;'>&quot;run&quot;</span>  <span style='color:#898887;'>// The compiler error should include &quot;run&quot;.</span>
  <span style='color:#04e040;'>exclude</span> <span style='color:#04e040;'>compiler</span> <span style='color:#bf0303;'>&quot;foo&quot;</span>  <span style='color:#898887;'>// The compiler error should not include &quot;foo&quot;.</span>
}

<span style='color:#898887;'>// You can include unittest when an error is expected; however, they will not be</span>
<span style='color:#898887;'>// run even if compilation succeeds.</span>

<b>define</b> <b><span style='color:#0057ae;'>MyType</span></b> {
  <span style='color:#898887;'>// Error! MyType does not have a definition for run.</span>
}

<b>concrete</b> <b><span style='color:#0057ae;'>MyType</span></b> {
  <span style='color:#644a9b;'>@type</span> run () -&gt; ()
}


<b><span style='color:#bf0303;background:#f7e6e6;'>testcase</span></b> <span style='color:#bf0303;'>&quot;intentional crash&quot;</span> {
  <span style='color:#898887;'>// The test is expected to crash.</span>
  <span style='color:#04e040;'>crash</span>
  <span style='color:#04e040;'>require</span> <span style='color:#04e040;'>stderr</span> <span style='color:#bf0303;'>&quot;message&quot;</span>  <span style='color:#898887;'>// stderr should include &quot;message&quot;.</span>
}

<span style='color:#898887;'>// Exactly one unittest must be defined when a crash is expected.</span>

<b>unittest</b> <b><span style='color:#0057ae;'>myTest</span></b> {
  <span style='color:#898887;'>// Use the fail built-in to cause a test failure.</span>
  <b>fail</b>(<span style='color:#bf0303;'>&quot;message&quot;</span>)
}
</pre>

Unit tests have access to all public symbols in the module. You can run all
tests for module `myprogram` using `zeolite -t myprogram`.

Specific things to keep in mind with `testcase`:

- All individual `unittest` have a default timeout of 30 seconds. This is to
  prevent automated test runners from hanging indefinitely if there is a
  deadlock or an infinite loop. This can be changed with `timeout t`, where `t`
  is specified in seconds. Specifing `timeout 0` disables the time limit.
- To simplify parsing, there are a few limitations with how you order the fields
  in a `testcase`:
  - The expected outcome (`success`, `crash`, `error`, `compiles`) must be at
    the top.
  - All `require` and `exclude` patterns must be grouped together. (Put another
    way, if a field other than `require` or `exclude` follows one of those two,
    the parser will move on.)

### Code Coverage

As of compiler version `0.16.0.0`, you can get a log of all lines of Zeolite
code (from `.0rx` or `.0rt` sources) with the `--log-traces `*`[filename]`*
option when running tests with `zeolite -t`.

- If *`[filename]`* is not an absolute path, it will be created relative to the
  path specified with `-p` if used. *The file will be overwritten*, and will
  contain all traces from a single call to `zeolite -t`.

- The current format is `.csv` with the following columns (includes a header):

  - `"microseconds"`: The call time from a monotonic microseconds timer, with an
    unspecified starting point.
  - `"pid"`: A unique ID for the current process. This *is not* the real process
    ID from the system, since that will often not be unique.
  - `"function"`: The name of the function where the line was executed.
  - `"context"`: The source-code context (e.g., file, line) of the call. The
    context has the same format as stack traces for crashes.

- If a procedure uses the [`$NoTrace$` pragma](#procedure-pragmas), there will
  be no trace information in the log for that procedure. This is because the
  logging uses the same tooling that is used for stack traces.

- Nothing will be logged for `testcase` that use `compiles` or `error`, since
  those modes do not actually execute any compiled code.

- Keep in mind that the simple act of processing text for logging can obscure
  race conditions in a program; therefore, `--log-traces` should be skipped when
  troubleshooting race conditions.

- Check the size of the log file before attempting to open it in a desktop
  application. In many cases, it will be too large to display.

  If you just want code coverage, you can try something like this, to get just
  the unique lines executed from `.0rx` sources:

  ```shell
  sed 1d trace-log.csv | cut -d, -f3,4 | grep -v 0rt | sort -u
  ```

As of compiler version `0.20.0.0`, `zeolite -r` will cache information about the
possible `.0rx` lines that can show up in `--log-traces` mode. You can access
this information using `zeolite --show-traces `*`[module path]`*. This can then
be compared to the `"context"` field in the output `.csv` to determine if any
code was missed by the executed tests.

## Compiler Pragmas and Macros

(As of compiler version `0.5.0.0`.)

Pragmas allow compiler-specific directives within source files that do not
otherwise need to be a part of the language syntax. Macros have the same format,
and are used to insert code *after* parsing but *before* compilation.

The syntax for both is `$SomePragma$` (no options) or
`$AnotherPragma[`*`OPTIONS`*`]$` (uses pragma-specific options). The syntax for
_`OPTIONS`_ depends on the pragma being used. Pragmas are specific to the
context they are used in.

### Source File Pragmas

These must be at the top of the source file, before declaring or defining
categories or `testcase`s.

- **`$ModuleOnly$`**. This can only be used in `.0rp` files. It takes an
  otherwise-public source file and limits visibility to the module. (This is
  similar to package-private in Java.)

- **`$TestsOnly$`**. This can be used in `.0rp` and `.0rx` files. When used,
  the file is only visible to other sources that use it, as well as `.0rt`
  sources. `.0rp` sources still remain public unless `$ModuleOnly$` is used.
  The transitive effect of `$TestsOnly$` is preventing the use of particular
  categories in output binaries.

### Procedure Pragmas

These must occur at the very top of a function definition.

- **`$NoTrace$`**. (As of compiler version `0.6.0.0`.) Disables stack-tracing
  within this procedure. This is useful for recursive functions, so that trace
  information does not take up stack space. This does not affect tracing for
  functions that are called from within the procedure.

- **`$TraceCreation$`**. (As of compiler version `0.6.0.0`.) Includes a trace
  of the value's creation when the given `@value` function is called. If
  multiple functions in a call stack use `$TraceCreation$`, only the trace
  from the bottom-most function will be included.

  `$TraceCreation$` is useful when the context that the value was created in is
  relevant when debugging crashes. The added execution cost for the function is
  trivial; however, it increases the memory size of the value by a few bytes per
  call currently on the stack at the time it gets created.

### Local Variable Rules

These pragmas alter how variables are dealt with locally:

- **`$ReadOnly[`**_`var1,var2,...`_**`]$`**. (As of compiler version
  `0.13.0.0`.) Marks `var1`, `var2`, etc. as read-only for the remainder of the
  statements in this context. Note that this only prevents *assignment* to the
  variable; it does not prevent making calls to functions that change the state
  of the underlying value. It also does not prevent calls to other functions
  modifying the variable.

  <pre style='color:#1f1c1b;background-color:#f6f8fa;'>
  <b>scoped</b> {
    <i><span style='color:#0057ae;'>Int</span></i> i &lt;- <span style='color:#b08000;'>0</span>
  } <b>in</b> <b>while</b> (i &lt; <span style='color:#b08000;'>100</span>) {
    <span style='color:#898887;'>// i can still be overwritten here</span>
    <b><i><span style='color:#8060c0;'>$ReadOnly[</span></i></b><i><span style='color:#8060c0;'>i</span></i><b><i><span style='color:#8060c0;'>]$</span></i></b>
    <span style='color:#898887;'>// i can not be overwritten below here within the while block</span>
  } <b>update</b> {
    <span style='color:#898887;'>// this is fine because it's in a different scope</span>
    i &lt;- i+<span style='color:#b08000;'>1</span>
  }</pre>

  This can be used for any variable name visible in the current scope, including
  `@value` and `@category` members and argument and return variables.

- **`$Hidden[`**_`var1,var2,...`_**`]$`**. (As of compiler version `0.13.0.0`.)
  This works the same way as `ReadOnly` except that it also makes the variables
  inaccessible for reading. Note that this *does not* allow you to reuse a
  variable name; the variable name remains reserved.

- As of compiler version `0.16.0.0` both `$ReadOnly[...]$` and `$Hidden[...]$`
  can be used at the top of a `define` for a `concrete` category to protect
  member variables.

  <pre style='color:#1f1c1b;background-color:#f6f8fa;'>
  <b>define</b> <b><span style='color:#0057ae;'>Type</span></b> {
    <b><i><span style='color:#8060c0;'>$Hidden[</span></i></b><i><span style='color:#8060c0;'>foo</span></i><b><i><span style='color:#8060c0;'>]$</span></i></b>
    <b><i><span style='color:#8060c0;'>$ReadOnly[</span></i></b><i><span style='color:#8060c0;'>bar</span></i><b><i><span style='color:#8060c0;'>]$</span></i></b>

    <span style='color:#644a9b;'>@category</span> <i><span style='color:#0057ae;'>Int</span></i> foo &lt;- <span style='color:#b08000;'>1</span>
    <span style='color:#898887;'>// foo can still be read here for the purposes of initialization.</span>
    <span style='color:#644a9b;'>@category</span> <i><span style='color:#0057ae;'>Int</span></i> bar &lt;- foo
  }</pre>

Zeolite uses pragmas instead of something like `final` in Java for a few
reasons:

- In practice, a large percentage of variables to be treated as read-only
  require some sort of iterative or conditional setup. Marking the variable as
  `final` at declaration time would require creating a helper to initialize it.

  <pre style='color:#1f1c1b;background-color:#f6f8fa;'>
  <i><span style='color:#0057ae;'>Int</span></i> nextPow2 &lt;- <span style='color:#b08000;'>1</span>
  <b>while</b> (nextPow2 &lt; n) {
    nextPow2 &lt;- <span style='color:#b08000;'>2</span>*nextPow2
  }
  <b><i><span style='color:#8060c0;'>$ReadOnly[</span></i></b><i><span style='color:#8060c0;'>nextPow2</span></i><b><i><span style='color:#8060c0;'>]$</span></i></b></pre>

- A frequent source of errors in any language is multiple variables of the same
  type in the same scope. Marking some of them as read-only (or as hidden)
  within a certain context helps prevent errors caused by inadvertently using
  the wrong variable.

- If your code works correctly *with* variables marked as read-only or as
  hidden, then it will also work correctly *without* such markings; the markings
  are only there to *cause* compile-time errors. This means that they add
  limited value to interpreting the code, and can therefore be kept separate
  from the respective variable's definition without loss of clarity.

### Expression Macros

These can be used in place of language expressions.

- **`$SourceContext$`**. (As of compiler version `0.7.1.0`.) Inserts a
  `String`-literal with information about the macro's location within the
  source file. Note that if this is used within an expression macro in
  `.zeolite-module` (see `ExprLookup` below), the context will be within the
  `.zeolite-module` file itself. (Remember that macro substitution *is not* a
  preprocessor stage, unlike the C preprocessor.)

- **`$ExprLookup[`**_`MACRO_NAME`_**`]$`**. (As of compiler version
  `0.6.0.0`.) This directly substitutes in a language expression, as if it was
  parsed from that exact code location. _`MACRO_NAME`_ is the key used to look
  up the expression. Symbols will be resolved in the context that the
  substutition happens in.

  - **`MODULE_PATH`** is always defined. It is a `String`-literal containing
    the absolute path to the module owning the source file. This can be useful
    for locating data directories within your module independently of `$PWD`.

  - Custom macros can be included in the `.zeolite-module` for your module.
    This can be useful if your module requires different parameters from one
    system to another.

    <pre style='color:#1f1c1b;background-color:#f6f8fa;'>
    <span style='color:#898887;'>// my-module/.zeolite-module</span>

    <span style='color:#898887;'>// (Standard part of .zeolite-module.)</span>
    <b>path:</b> <span style='color:#bf0303;'>&quot;.&quot;</span>

    <span style='color:#898887;'>// Define your macros here.</span>
    <b>expression_map:</b> [
      <i>expression_macro</i> {
        <b>name:</b> <span style='color:#006e28;'>USE_DATA_VERSION</span>    <span style='color:#898887;'>// Access using $ExprLookup[USE_DATA_VERSION]$.</span>
        <b>expression:</b> <span style='color:#bf0303;'>&quot;2020-05-12&quot;</span>  <span style='color:#898887;'>// Substituted in as a Zeolite expression.</span>
      }
      <i>expression_macro</i> {
        <b>name:</b> <span style='color:#006e28;'>RECURSION_LIMIT</span>
        <b>expression:</b> <span style='color:#b08000;'>100000</span>
      }
      <i>expression_macro</i> {
        <b>name:</b> <span style='color:#006e28;'>SHOW_LIMIT</span>
        <span style='color:#898887;'>// All Zeolite expressions are allowed.</span>
        <b>expression:</b> <span style='color:#bf0303;'>&quot;limit: &quot;</span> + <b><i><span style='color:#8060c0;'>$ExprLookup[</span></i></b><i><span style='color:#8060c0;'>RECURSION_LIMIT</span></i><b><i><span style='color:#8060c0;'>]$</span></i></b>.formatted()
      }
    ]

    <span style='color:#898887;'>// (Standard part of .zeolite-module.)</span>
    <b>mode:</b> <i>incremental</i> {}
    </pre>

    The `name:` must only contain uppercase letters, numbers, and `_`, and the
    `expression:` must parse as a valid Zeolite expression. This is similar to
    C++ macros, except that the substitution must be *independently* parsable
    as a valid expression, and it can only be used where expressions are
    otherwise allowed.

## Known Language Limitations

(Previous limitations that were listed here have been solved.)

[action-status]: https://github.com/ta0kira/zeolite/actions/workflows/haskell-ci.yml/badge.svg
[action-zeolite]: https://github.com/ta0kira/zeolite/actions/workflows/haskell-ci.yml
[cabal]: https://www.haskell.org/cabal/#install-upgrade
[category]: https://en.wikipedia.org/wiki/Category_theory
[clang]: https://clang.llvm.org/cxx_status.html
[examples]: https://github.com/ta0kira/zeolite/tree/master/example
[gcc]: https://gcc.gnu.org/
[ghc]: https://www.haskell.org/ghc/
[hackage]: http://hackage.haskell.org
[hackage-status]: https://img.shields.io/hackage/v/zeolite-lang.svg?style=flat
[hackage-zeolite-lang]: http://hackage.haskell.org/package/zeolite-lang
[initial-terminal]: https://en.wikipedia.org/wiki/Initial_and_terminal_objects
[kate]: https://kate-editor.org/
[lib]: https://github.com/ta0kira/zeolite/tree/master/lib
[lib/file]: https://github.com/ta0kira/zeolite/tree/master/lib/file
[procedural]: https://en.wikipedia.org/wiki/Procedural_programming
[oop]: https://en.wikipedia.org/wiki/Object-oriented_programming
[variance]: https://en.wikipedia.org/wiki/Covariance_and_contravariance_%28computer_science%29
[zeolite]: https://github.com/ta0kira/zeolite
[zeolite-issues]: https://github.com/ta0kira/zeolite/issues
[zeolite.xml]: https://github.com/ta0kira/zeolite/blob/master/highlighting/kate/zeolite.xml
