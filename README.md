# Zeolite Programming Language

Zeolite is a statically-typed, general-purpose programming language. The type
system revolves around defining objects and their usage patterns.

Zeolite prioritizes making code written with it simple to understand for readers
who didn't write the original code. This is done by limiting flexibility in
some places and increasing it in others. In particular, emphasis is places on
the user's experience when troubleshooting code that is *incorrect*.

The design of the type system and the language itself is influenced by positive
and negative experiences with Java, C++, Haskell, Python, and Go, with
collaborative development, and with various systems of code-quality enforcement.

## Hello World

It's the [any%](https://en.wiktionary.org/wiki/any%25) of programming.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<span style='color:#898887;'>// helloworld/hello-world.0rx</span>

<b>concrete</b> <b><span style='color:#0057ae;'>HelloWorld</span></b> {
  <span style='color:#644a9b;'>@type</span> run () <b><span style='color:#006e28;'>-&gt;</span></b> ()
}

<b>define</b> <b><span style='color:#0057ae;'>HelloWorld</span></b> {
  run () {
    <span style='color:#006e28;'>~</span> <span style='color:#0057ae;'>LazyStream</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>Formatted</span></i><span style='color:#c02040;'>&gt;</span><span style='color:#644a9b;'>$</span>new().append(<span style='color:#bf0303;'>&quot;Hello World</span><span style='color:#924c9d;'>\n</span><span style='color:#bf0303;'>&quot;</span>).writeTo(<span style='color:#0057ae;'>SimpleOutput</span><span style='color:#644a9b;'>$</span>stderr())
  }
}</pre>

```shell
# Compile.
./zeolite -i util -m HelloWorld helloworld

# Execute.
./HelloWorld
```

Also see a [full example][example] for more complete feature usage.

## Project Status

The Zeolite compiler is currently a prototype that compiles source files into
C++, which can then be compiled with a C++ compiler. The primary use-case at the
moment is to experiment with a new type system, described in the next section.
A full compiler will be written if the language turns out to be useful.

The project is currently lacking most standard-library functionality, but it can
be easily extended using C++ to access C and C++ libraries. Since a compiler
rewrite will change how extension is done, standard-library work will likely be
minimal.

## Motivation

The type system used by Zeolite is based on
[**category theory**](https://en.wikipedia.org/wiki/Category_%28mathematics%29).
This is essentially a mathematical formalism that allows us to prove that
particular type conversions and function calls are safe. Overall, this knowledge
is not needed in order to use the language.

Everything in Zeolite starts with **parameterized types** called **type
cateories**, which are similar to classes in generics Java and templates in C++.
The primary design objective of the type system is to impose the least amount of
effort on the users of types and callers of functions. This is done by allowing
implicit type conversions in situations where languages like Java and C++ would
not.

### Parameter Variance - Java

Take the following Java code as an example:

```java
// Java! Not Zeolite.

void addItems(List<? super X> list);
void readItems(List<? extends X> list);
```

In the code above, `addItems` can add things to `list` that have `X` as a
super-class. For `readItems`, `list` can contain any type of element, as long as
it has `X` as a super-class.

Although this is a powerful feature (vs. C++ templates), it is still suboptimal:

- In `addItems`, the use of `super` means that all *read* functionality in the
  `List` is unusable.
- In `readItems`, the use of `extends` means that all *write* functionality in
  the `List` is unusable.

The issue in both cases is that the caller must pass an object that implements
functionality that is *provably unused* by the method.

### Parameter Variance - Zeolite

Zeolite has a similar approach to that of C# when it comes to handling parameter
variance: It allows the author of the type to assign a fixed **variance rule**
to each parameter, and then automatically handles conversions between
substitutions that have compatible parameters.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Writer</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c04040;'>|</span><span style='color:#c02040;'>&gt;</span> {  <span style='color:#898887;'>// &lt;- &lt;#x|&gt; means param #x is contravariant</span>
  append (<i><span style='color:#0057ae;'>#x</span></i>) <b><span style='color:#006e28;'>-&gt;</span></b> ()
}

<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Reader</span></b><span style='color:#c02040;'>&lt;</span><span style='color:#c04040;'>|</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {  <span style='color:#898887;'>// &lt;- &lt;|#x&gt; means param #x is covariant</span>
  iterator () <b><span style='color:#006e28;'>-&gt;</span></b> (<b>optional</b> <span style='color:#0057ae;'>Iterator</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span>)
}

<b>concrete</b> <b><span style='color:#0057ae;'>List</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  <b>refines</b> <span style='color:#0057ae;'>Writer</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span>
  <b>refines</b> <span style='color:#0057ae;'>Reader</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span>
}

<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Iterator</span></b><span style='color:#c02040;'>&lt;</span><span style='color:#c04040;'>|</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  get () <b><span style='color:#006e28;'>-&gt;</span></b> (<i><span style='color:#0057ae;'>#x</span></i>)
  next () <b><span style='color:#006e28;'>-&gt;</span></b> (<b>optional</b> <span style='color:#0057ae;'>Iterator</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span>)
}</pre>

`Writer` has a **contravariant** parameter `#x`, which means that any `Writer`
can be used as a `Writer` of a *more specific* type of object. Similarly,
`Reader` has a **covariant** parameter `#x`, which means that any `Reader` can
be used as a `Reader` of a *more general* type of object.

The Zeolite equivalent to the Java example is then:

<pre style='color:#1f1c1b;background-color:#ffffff;'>
addItems (<span style='color:#0057ae;'>Writer</span><span style='color:#c02040;'>&lt;</span><span style='color:#0057ae;'>X</span><span style='color:#c02040;'>&gt;</span>) <b><span style='color:#006e28;'>-&gt;</span></b> ()
readItems (<span style='color:#0057ae;'>Reader</span><span style='color:#c02040;'>&lt;</span><span style='color:#0057ae;'>X</span><span style='color:#c02040;'>&gt;</span>) <b><span style='color:#006e28;'>-&gt;</span></b> ()</pre>

This means that:

- Any `Writer` can be passed to `addItems`, as long as the original `Writer`
  type is *more general* than `X`.
- Any `Reader` can be passed to `readItems`, as long as the original `Reader`
  type is *more specific* than `X`.

For example:

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Parent</span></b> {}

<b>concrete</b> <b><span style='color:#0057ae;'>Child</span></b> {
  <b>refines</b> <span style='color:#0057ae;'>Parent</span>
}

<span style='color:#898887;'>// ... glossing over how functions are defined ...</span>

addItems (<span style='color:#0057ae;'>Writer</span><span style='color:#c02040;'>&lt;</span><span style='color:#0057ae;'>Child</span><span style='color:#c02040;'>&gt;</span>) <b><span style='color:#006e28;'>-&gt;</span></b> ()
readItems (<span style='color:#0057ae;'>Reader</span><span style='color:#c02040;'>&lt;</span><span style='color:#0057ae;'>Parent</span><span style='color:#c02040;'>&gt;</span>) <b><span style='color:#006e28;'>-&gt;</span></b> ()</pre>

- We could pass a `List<Parent>` to `addItems`, since `Child` values can be
  written to such a destination.
- We could pass a `List<Child>` to `readItems`, since such data source can give
  back `Parent` values.

Importantly, this conversion is handled automatically by the compiler, with no
additional syntax aside from `|` where the parameter is defined. (Also note that
the Java approach discussed in the previous section is *still possible* using
parameter filters.)

## Language Overview

The central goal of the langauge design is to limit the possible interpretations
of a given piece of code, *and* limit the possible interpretations of
compile-time errors.

- All procedural code exists at the very bottom of the inheritance graph, in
  **concrete types**. These *cannot* be further refined, but they can implement
  any number of **value interfaces** and **type interfaces** to express *usage
  patterns*.
- Data members in concrete types have *internal* visibility, meaning that they
  are not accessible even by other members of the same type. Incidentally, this
  means that there are no publicly-visible data members *anywhere*.
- Multiple functions with the same name in the same type (i.e., "overloading")
  are *not allowed*. This is primarily to limit the amount of type inference
  done by the compiler, thereby making it simpler for the user to reason about.
- There are no constructors and there is no default initialization; values can
  only be created using factory functions or literals. This encourages more
  focus on "what it *does*" rather than "what it *is*".
- Variables can only be missing (similar to `null` in Java) if a special storage
  type is used.

Zeolite also *increases* flexbility in some ways:

- **Type interfaces** can be used for requiring type-level functions. This
  allows procedures to call functions on type parameters themselves. This
  would be like Java allowing abstract `static` methods.
- Constraints on type parameters can be used when declaring categories or
  functions, to ensure that the parameter can be used in a certain manner. This
  is similar to how Java allows syntax like `<X extends Parent>`, but it is done
  out of line so that constraints can contain more complex relationships between
  parameters.
- The types used in inherited functions can be overridden using compatible
  types. Additionally, multiple inherited functions with the same name can be
  **merged** into a single function if they have compatible types.

## Getting Started

### Installing Zeolite

Zeolite is currently developed and tested on Linux. It might not work out of the
box on other operating systems at the moment.

1. Make sure that you have [`git`][git], [`clang++`][clang] (possibly installed
   via a package just named "clang"), and [`ghc`][ghc] installed on your system.
2. Clone this project in a local directory.
   ```shell
   git clone https://github.com/ta0kira/zeolite.git
   ```
3. Update the submodules used by this project.
   ```shell
   cd zeolite && git submodule init && git submodule update
   ```
4. Install a few Haskell dependencies via `cabal`.
   ```shell
   sudo apt-get install cabal-install
   cabal update
   cabal install mtl parsec regex-tdfa
   ```
5. Run the compiler setup:
   ```shell
   ./setup.sh
   ```
   This will create the compiler binary `./zeolite`. If you modify the compiler
   code or move the source directory, you will need to rerun `setup.sh`.

`zeolite` was written for Linux systems that have the [`clang++`][clang] C++
compiler and the [`ghc`][ghc] Haskell compiler. More flexibility will probably
be added in the future.

If you feel like running the unit and integration tests, you can run the
commands below. This isn't required, but it might be useful if you modify the
compiler.

```shell
# Unit Tests.
( cd compiler && ghc all-tests.hs && ./all-tests )

# Integration Tests.
./zeolite -i util -c tests
./zeolite -t tests
```

### Source Files

There are two distinct types of source file used by the Zeolite compiler:

1. `.0rx`: Implementation files that can contain category declarations, i.e.,
   `concrete`, `@value interface`, `@type interface`, and category definitions,
   i.e., `define`. Nothing in implementation files is visible to other source
   files.
2. `.0rp`: Declaration files that can only contain category declarations; they
   *cannot* contain `define`. Everything in declaration files is visible to
   other source files.

If you want to have an entire program in a single source file, use `.0rx`. If
you need to split it up, you must also use `.0rp` for sharing type declarations.

### Compiling Programs

To create a program, define a `concrete` category and implement the
`@type run () -> ()` function. (The function signature means that it takes
nothing and returns nothing, and it is called on a *type* rather than a
*value*.)

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<span style='color:#898887;'>// yourprojectdir/your-source.0rx</span>

<b>concrete</b> <b><span style='color:#0057ae;'>YourCategory</span></b> {
  <span style='color:#644a9b;'>@type</span> run () <b><span style='color:#006e28;'>-&gt;</span></b> ()
}

<b>define</b> <b><span style='color:#0057ae;'>YourCategory</span></b> {
  run () { <span style='color:#898887;'>/*</span><span style='color:#898887;'> your code </span><span style='color:#898887;'>*/</span> }
}</pre>

```shell
# Compile.
./zeolite -i util -m YourCategory yourprojectdir

# Execute.
./YourCategory
```

The `zeolite` compiler operates on *source paths* rather than on *source files*.
Unlike C++, you cannot specify individual source files to compile; all of the
source files (i.e., `.0rp` and `.0rx`) at the top level of the path are
included. This allows `zeolite` to more easily manage dependencies between
source files during compilation.

To compile the module into a binary, call `zeolite` with the `-m` option,
passing the name of the main category. The default binary name is the same as
that of the category, placed in the current directory. Additional dependencies
can be included using `-i`. You can call `./zeolite -h` for the most-current
options.

## Basic Ideas

This section provides a language intro, starting with simple topics and getting
more advanced.

### Concrete Types, and Procedures

If you learn nothing else here, you at least need to be able to write something
that will execute. **Concrete** categories are the only place you can define
procedural code.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>concrete</b> <b><span style='color:#0057ae;'>MyType</span></b> {
  <span style='color:#644a9b;'>@value</span> func () <b><span style='color:#006e28;'>-&gt;</span></b> ()
  <span style='color:#644a9b;'>@type</span> new () <b><span style='color:#006e28;'>-&gt;</span></b> (<span style='color:#0057ae;'>MyType</span>)
}

<b>define</b> <b><span style='color:#0057ae;'>MyType</span></b> {
  <span style='color:#644a9b;'>@value</span> <i><span style='color:#0057ae;'>Int</span></i> x

  func () {
    <span style='color:#006e28;'>~</span> func2(<span style='color:#b08000;'>123</span>)
  }

  new () {
    <b>return</b> <span style='color:#0057ae;'>MyType</span>{ <span style='color:#b08000;'>0</span> }
  }

  <span style='color:#644a9b;'>@value</span> func2 (<i><span style='color:#0057ae;'>Int</span></i>) <b><span style='color:#006e28;'>-&gt;</span></b> ()
  func2 (y) {
    x <b><span style='color:#006e28;'>&lt;-</span></b> y
  }
}</pre>

From top to bottom:

- `concrete` followed by a name that starts with an *uppercase* letter lets the
  compiler know that a new category is being declared. This category has no
  type parameters.
- `@value` means that what follows is specific to **allocated values** from the
  category. (Like instances in Java and C++.)
- The function `func` takes no arguments and returns nothing.
- `@type` means that what follows applies to **type instances**. In this case,
  `new` returns an allocated `MyType`. (This is like a static function in C++.)
  There are no constructors in Zeolite, so you need a function like this if you
  want to create values.
- `define` followed by an existing category name starts the definition of its
  members and procedures. *Do not* specify type parameters here, since that
  would be redundant and error-prone.
- `@value Int x` at the top of the definition means that each allocated value of
  type `MyType` has this data member.
- Procedures do not need to repeat what the argument and return types are since,
  again, that would be redundant and error-prone.
- The `~` before `func2(123)` just means that you don't care what the result of
  the call is, nothing is being assigned, and nothing is being returned.
- `new` initializes a new value, setting `x` to `0`. This is the only time a
  `@type` function has direct access to `@value` members.
- An internal-only function `func2` is declared within the definition, with a
  definition immediately following.

### `@category`, `@type`, and `@value`

Functions and non-local variables in Zeolite can have scoping rules that are
similar to `static` in C++ and Java. (Note that `static` has different semantics
in C++ vs. Java.)

Take the following category as an example:

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>concrete</b> <b><span style='color:#0057ae;'>Category</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  <span style='color:#644a9b;'>@category</span> create&lt;<i><span style='color:#0057ae;'>#y</span></i>&gt; (<i><span style='color:#0057ae;'>#y</span></i>) <b><span style='color:#006e28;'>-&gt;</span></b> (<span style='color:#0057ae;'>Category</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#y</span></i><span style='color:#c02040;'>&gt;</span>)
  <span style='color:#644a9b;'>@type</span>     new () <b><span style='color:#006e28;'>-&gt;</span></b> (<span style='color:#0057ae;'>Category</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span>)
  <span style='color:#644a9b;'>@value</span>    get () <b><span style='color:#006e28;'>-&gt;</span></b> (<i><span style='color:#0057ae;'>#x</span></i>)
}</pre>

- `create` is a `@category` function, which means that you call it from the
  category name *without* a type parameter. This is done with `$$`, e.g.,
  `Category$$create<String>("string")`. `@category` functions don't have access
  to the type parameters, much like `static` methods in Java.

- `new` is a `@type` function, which means that you call it from a type instance
  *with* all type parameters. This is done with `$`, e.g.,
  `Category<String>$new()`. This is more like `static` in C++.

- `get` is a `@value` function, which requires an allocated value to call, e.g.,
  `String y <- x.get()`. This is like member functions in C++ and Java.

This scoping can also be applied to variables, which are discussed in the next
section.

### Variables

All variables must be initialized where they are defined *except* for `@value`
variables and named returns. The compiler still requires initialization in both
of those cases, but it's done out of line.

Variables are assigned values with `<-`, or are assigned positionally when
initializing `@value` members. (`=` is not used because that would imply that
the left and right could be swapped.)

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>concrete</b> <b><span style='color:#0057ae;'>Type</span></b> {
  <span style='color:#644a9b;'>@type</span> create (<i><span style='color:#0057ae;'>Int</span></i>) <b><span style='color:#006e28;'>-&gt;</span></b> (<span style='color:#0057ae;'>Type</span>)
  <span style='color:#644a9b;'>@value</span> get () <b><span style='color:#006e28;'>-&gt;</span></b> (<i><span style='color:#0057ae;'>Int</span></i>)
}

<b>define</b> <b><span style='color:#0057ae;'>Type</span></b> {
  <span style='color:#644a9b;'>@value</span> <i><span style='color:#0057ae;'>Int</span></i> x  <span style='color:#898887;'>// x isn't initialized here.</span>
  <span style='color:#644a9b;'>@category</span> <i><span style='color:#0057ae;'>Bool</span></i> z <b><span style='color:#006e28;'>&lt;-</span></b> <b>true</b>  <span style='color:#898887;'>// @category variables are initialized in place.</span>
  <span style='color:#898887;'>// @type Bool w &lt;- true  // @type variables are not allowed.</span>

  create (y) {
    <b>return</b> <span style='color:#0057ae;'>Type</span>{ y }  <span style='color:#898887;'>// x is initialized here.</span>
  }

  get () (y) {  <span style='color:#898887;'>// y isn't initialized here.</span>
    y <b><span style='color:#006e28;'>&lt;-</span></b> x  <span style='color:#898887;'>// y is initialized here.</span>
  }

  <span style='color:#644a9b;'>@value</span> something () <b><span style='color:#006e28;'>-&gt;</span></b> ()
  something () {
    <i><span style='color:#0057ae;'>Int</span></i> y <b><span style='color:#006e28;'>&lt;-</span></b> <span style='color:#b08000;'>0</span>  <span style='color:#898887;'>// Local variables are initialized in place.</span>
  }
}</pre>

Unlike most other languages, variable masking is not allowed. For example, if
there is a `@value` variable named `x` then no `@value` function can create a
local variabled named `x`.

Variables *cannot* be accessed with a qualifier. For example, the syntax
`val.member` (to access the `member` variable in `val`) isn't valid in Zeolite.
This is because it's unsafe to access members of another object without first
ensuring that variance rules are respected. (Also see the section about
parameter variance.)

### Multiple Returns

Functions can return more than one value. This can either be done by enclosing
values in `{}` or by naming them.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>concrete</b> <b><span style='color:#0057ae;'>Something</span></b> {
  <span style='color:#644a9b;'>@type</span> func1 () <b><span style='color:#006e28;'>-&gt;</span></b> (<i><span style='color:#0057ae;'>Int</span></i>,<i><span style='color:#0057ae;'>Int</span></i>)
  <span style='color:#644a9b;'>@type</span> func2 () <b><span style='color:#006e28;'>-&gt;</span></b> (<i><span style='color:#0057ae;'>Int</span></i>,<i><span style='color:#0057ae;'>Int</span></i>)
  <span style='color:#644a9b;'>@type</span> func3 () <b><span style='color:#006e28;'>-&gt;</span></b> (<i><span style='color:#0057ae;'>Int</span></i>,<i><span style='color:#0057ae;'>Int</span></i>)
}

<b>define</b> <b><span style='color:#0057ae;'>Something</span></b> {
  func1 () {
    <b>return</b> { <span style='color:#b08000;'>1</span>, <span style='color:#b08000;'>2</span> }
  }

  func2 () (v1,v2) {
    v1 <b><span style='color:#006e28;'>&lt;-</span></b> <span style='color:#b08000;'>1</span>
    <b>if</b> (something()) {
      v2 <b><span style='color:#006e28;'>&lt;-</span></b> <span style='color:#b08000;'>2</span>
    } <b>else</b> {
      v2 <b><span style='color:#006e28;'>&lt;-</span></b> <span style='color:#b08000;'>3</span>
    }
    <span style='color:#898887;'>// All names must be assigned by the time the function returns. To return</span>
    <span style='color:#898887;'>// early, use return _.</span>
  }

  func3 () (v1,v2) {
    <span style='color:#898887;'>// Positional returns are still fine even when names are used.</span>
    <b>return</b> { <span style='color:#b08000;'>3</span>, <span style='color:#b08000;'>4</span> }
  }

  <span style='color:#644a9b;'>@category</span> something () <b><span style='color:#006e28;'>-&gt;</span></b> (<i><span style='color:#0057ae;'>Bool</span></i>)
  something () {
    <b>return</b> <b>false</b>
  }
}</pre>

The choice depends on the situation:

- Unnamed returns are fine if it's easy to get all of the values in the same
  place at the same time. This also requires specifying all values at all of the
  return points.
- Named returns are helpful when the values are determined separately, which
  might otherwise require extra temporary variables. Explicit return statements
  are disallowed other than `return _` to return with the current assignments.
  You still have the option to use the semantics for unnamed returns.

There are also a few options for *receiving* multiple returns:

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>concrete</b> <b><span style='color:#0057ae;'>Something</span></b> {
  <span style='color:#644a9b;'>@type</span> func1 () <b><span style='color:#006e28;'>-&gt;</span></b> ()
}

<b>define</b> <b><span style='color:#0057ae;'>Something</span></b> {
  func1 () {
    <i><span style='color:#0057ae;'>Int</span></i> x <b><span style='color:#006e28;'>&lt;-</span></b> <span style='color:#b08000;'>0</span>
    <span style='color:#898887;'>// Assign to existing (x) and new (y) variables.</span>
    { x, <i><span style='color:#0057ae;'>Int</span></i> y } <b><span style='color:#006e28;'>&lt;-</span></b> func2()
    <span style='color:#898887;'>// Ignore the first return value.</span>
    { <b>_</b>, y } <b><span style='color:#006e28;'>&lt;-</span></b> func2()
    <span style='color:#898887;'>// Ignore all return values.</span>
    <span style='color:#006e28;'>~</span> func2()
  }

  <span style='color:#644a9b;'>@type</span> func2 () <b><span style='color:#006e28;'>-&gt;</span></b> (<i><span style='color:#0057ae;'>Int</span></i>,<i><span style='color:#0057ae;'>Int</span></i>)
  func2 () {
    <b>return</b> { <span style='color:#b08000;'>1</span>, <span style='color:#b08000;'>2</span> }
  }
}</pre>

Lastly, if a function returns _n_ values, you can directly pass those to another
function that takes *n* arguments:

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<span style='color:#644a9b;'>@type</span> get () <b><span style='color:#006e28;'>-&gt;</span></b> (<i><span style='color:#0057ae;'>Int</span></i>,<i><span style='color:#0057ae;'>Int</span></i>)

<span style='color:#644a9b;'>@type</span> call (<i><span style='color:#0057ae;'>Int</span></i>,<i><span style='color:#0057ae;'>Int</span></i>) <b><span style='color:#006e28;'>-&gt;</span></b> ()

<span style='color:#898887;'>// ...</span>

<span style='color:#006e28;'>~</span> call(get())</pre>

Concatenation of multiple returns with other arguments *isn't* done, just to
avoid confusing function calls.

### Value Interfaces

Each `@value interface` specifies `@value` functions to be inherited. These are
just function signatures (like `interface` methods in Java) that must be given a
procedure if inherited by a `concrete` category.

Concrete categories and other value interfaces can both inherit value interfaces
using `refines`.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Printable</span></b> {
  print () <b><span style='color:#006e28;'>-&gt;</span></b> (<i><span style='color:#0057ae;'>String</span></i>)
}

<b>concrete</b> <b><span style='color:#0057ae;'>Data</span></b> {
  <b>refines</b> <span style='color:#0057ae;'>Printable</span>
}

<b>define</b> <b><span style='color:#0057ae;'>Data</span></b> {
  print () {
    <b>return</b> <span style='color:#bf0303;'>&quot;Data&quot;</span>
  }
}</pre>

Here `Data` refines `Printable`, and so it must provide a procedure for it. Note
that `@value` is *not* needed in the interface.

Argument and return types can be overridden when refining, as long as the types
are compatible with the originals. Overriding can be done in either the
declaration or definition.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Parent1</span></b> {}

<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Parent2</span></b> {
  <b>refines</b> <span style='color:#0057ae;'>Parent1</span>
}

<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>GetParent</span></b> {
  getParent () <b><span style='color:#006e28;'>-&gt;</span></b> (<span style='color:#0057ae;'>Parent1</span>)
}

<b>concrete</b> <b><span style='color:#0057ae;'>Data</span></b> {
  <b>refines</b> <span style='color:#0057ae;'>Parent2</span>
  <b>refines</b> <span style='color:#0057ae;'>GetParent</span>

  <span style='color:#898887;'>// The return is more specific than the original Parent1.</span>
  <span style='color:#644a9b;'>@value</span> getParent () <b><span style='color:#006e28;'>-&gt;</span></b> (<span style='color:#0057ae;'>Parent2</span>)
}

<b>define</b> <b><span style='color:#0057ae;'>Data</span></b> {
  <span style='color:#898887;'>// The return is more specific than Parent2.</span>
  <span style='color:#644a9b;'>@value</span> getParent () <b><span style='color:#006e28;'>-&gt;</span></b> (<span style='color:#0057ae;'>Data</span>)
  getParent () {
    <b>return</b> <b>self</b>
  }
}</pre>

### Type Interfaces

A `@type interface` is similar to a `@value interface`, except that:

- They specify `@type` functions that must be defined.
- Inheritance is done using `defines`.
- Type interfaces *cannot* inherit other type interfaces.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<span style='color:#644a9b;'>@type</span> <b>interface</b> <b><span style='color:#0057ae;'>Factory</span></b><span style='color:#c02040;'>&lt;</span><span style='color:#c04040;'>|</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  create () <b><span style='color:#006e28;'>-&gt;</span></b> (<i><span style='color:#0057ae;'>#x</span></i>)
}

<b>concrete</b> <b><span style='color:#0057ae;'>Data</span></b> {
  <b>defines</b> <span style='color:#0057ae;'>Factory</span><span style='color:#c02040;'>&lt;</span><span style='color:#0057ae;'>Data</span><span style='color:#c02040;'>&gt;</span>
}

<b>define</b> <b><span style='color:#0057ae;'>Data</span></b> {
  create () {
    <b>return</b> <span style='color:#0057ae;'>Data</span>{}
  }
}</pre>

### Value Interfaces vs. Type Interfaces

The choice between `@value interface` and `@type interface` should be based on
two things:

1. Will the functions operate on *values* of the category that defines the
   procedure? If not, then it should be a `@type interface`. For example,
   factory functions create *new* values (e.g., `Type$new()`) and are therefore
   called before a value exists.
2. Should the procedure be determined by the *runtime* type of a particular
   value? That might not always be desirable. For example, in Java,
   `x.equals(y)` might use a different procedure than `y.equals(x)` because the
   *runtime* types of `x` and `y` (respectively) determine how the call is
   dispatched.

The second point above is why the `LessThan` and `Equals` built-in categories
are `@type interface`. This ensures that the procedure is chosen based on a
*type instance* and not on a *runtime type* of one of its arguments.

The main drawback of type interfaces is that their procedures can only be
defined for `concrete` categories. This means that, for example, a filter such
as `#x defines Equals<#x>` cannot be satisfied by a `@value interface`. (Also
see the section about parameter filters.)

### Control Flow

Zeolite provides `if`/`elif`/`else` and `while` constructs.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>if</b> (x) {
  <span style='color:#898887;'>// something</span>
} <b>elif</b> (y) {
  <span style='color:#898887;'>// something</span>
} <b>else</b> {
  <span style='color:#898887;'>// something</span>
}

<b>while</b> (x &gt; <span style='color:#b08000;'>0</span>) {
  <span style='color:#898887;'>// something</span>
  <b>break</b>    <span style='color:#898887;'>// exit the loop</span>
  <b>continue</b> <span style='color:#898887;'>// next iteration</span>
}

<b>while</b> (x &gt; <span style='color:#b08000;'>0</span>) {
  <span style='color:#898887;'>// something</span>
} <b>update</b> {
  <span style='color:#898887;'>// before next iteration</span>
}</pre>

`for` loops are not a built-in syntax because such loops are a very specialized
case of `while`, and can become ugly very quickly if additional flexibility is
required. The same functionality can be achieved with `update` and `scoped`
statements, discussed in the next section.

### Scoped Statements

Temporary variables are often only needed for a single statement (or control
block) and can then be discarded. Rather than leaving them lying around, you can
use a `scoped` statement.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>scoped</b> {
  <i><span style='color:#0057ae;'>Int</span></i> diff <b><span style='color:#006e28;'>&lt;-</span></b> getDiff()
} <b>in</b> <b>if</b> (diff &lt; <span style='color:#b08000;'>0</span>) {
  <span style='color:#898887;'>// something</span>
} <b>elif</b> (diff &gt; <span style='color:#b08000;'>0</span>) {
  <span style='color:#898887;'>// something</span>
}
<span style='color:#898887;'>// diff does not exist here</span>

<b>scoped</b> {
  { <i><span style='color:#0057ae;'>Int</span></i> x, <i><span style='color:#0057ae;'>Int</span></i> y } <b><span style='color:#006e28;'>&lt;-</span></b> getVals()
} <b>in</b> <i><span style='color:#0057ae;'>Int</span></i> z <b><span style='color:#006e28;'>&lt;-</span></b> x+y
<span style='color:#898887;'>// x and y don't exist here</span></pre>

There is currently no `for` loop syntax, but one can be created using `scoped`
and `while`.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>scoped</b> {
  <i><span style='color:#0057ae;'>Int</span></i> i <b><span style='color:#006e28;'>&lt;-</span></b> <span style='color:#b08000;'>0</span>
  <i><span style='color:#0057ae;'>Int</span></i> limit <b><span style='color:#006e28;'>&lt;-</span></b> <span style='color:#b08000;'>10</span>
} <b>in</b> <b>while</b> (i &lt; limit) {
  <span style='color:#898887;'>// something with i</span>
} <b>update</b> {
  i <b><span style='color:#006e28;'>&lt;-</span></b> i+<span style='color:#b08000;'>1</span>
}</pre>

### Optional and Weak Values

In Java, all (boxed) values can be `null`. In C++, all pointers can be
`nullptr`. In most cases, such values are invalid and must be checked for in
"clean" code.

Zeolite mitigates this by disallowing missing values unless the `optional`
storage modifier is used. Values with this qualifier *cannot* be used without
first converting them to a required value.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>optional</b> <span style='color:#0057ae;'>Value</span> value <b><span style='color:#006e28;'>&lt;-</span></b> getValue()
<b>if</b> (<b>present</b>(value)) {
  <b>require</b>(value).func()
}</pre>

`present` and `require` are built-in functions that operate on `optional`, and
are the only two operations that can be performed. There is only one value that
is not `present`, which is the literal `empty`. Calling `require` on an `empty`
value will cause a crash.

`weak` values are similar, but will disappear if no other non-`weak` references
exist. The only operation that can be performed on `weak` is `strong`, which
turns the value into an `optional`.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>weak</b> <span style='color:#0057ae;'>Value</span> value <b><span style='color:#006e28;'>&lt;-</span></b> <b>self</b>
<b>scoped</b> {
  <b>optional</b> <span style='color:#0057ae;'>Value</span> value2 <b><span style='color:#006e28;'>&lt;-</span></b> <b>strong</b>(value)
} <b>in</b> <b>if</b> (<b>present</b>(value2)) {
  <b>require</b>(value2).func()
}</pre>

`weak` values *cannot* be passed as arguments or returned from functions, but
they can be created locally or as members. Required and optional values can
automatically covert to `weak`.

## Type Parameters

Type parameters allow you consistently apply the same semantics to different
types of value. Categories and functions can both have type parameters.
Parameter names *must* start with `#` and a lowercase letter, e.g., `#x`.

When using type parameters for `concrete` categories, *do not* redeclare the
parameters in the `define`.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>concrete</b> <b><span style='color:#0057ae;'>Type</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {}

<b>define</b> <b><span style='color:#0057ae;'>Type</span></b> <span style='color:#898887;'>/*</span><span style='color:#898887;'>Do not use &lt;#x&gt; here.</span><span style='color:#898887;'>*/</span> {}</pre>

Type parameters in functions directly follow the function name.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>concrete</b> <b><span style='color:#0057ae;'>Type</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  <span style='color:#644a9b;'>@category</span> create&lt;<i><span style='color:#0057ae;'>#y</span></i>&gt; (<i><span style='color:#0057ae;'>#y</span></i>) <b><span style='color:#006e28;'>-&gt;</span></b> (<span style='color:#0057ae;'>Type</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#y</span></i><span style='color:#c02040;'>&gt;</span>)
}

<span style='color:#898887;'>// ...</span>

<span style='color:#0057ae;'>Type</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>String</span></i><span style='color:#c02040;'>&gt;</span> val <b><span style='color:#006e28;'>&lt;-</span></b> <span style='color:#0057ae;'>Type</span><span style='color:#644a9b;'>$$</span>create&lt;<i><span style='color:#0057ae;'>String</span></i>&gt;(<span style='color:#bf0303;'>&quot;string&quot;</span>)</pre>

### Parameter Variance

Parameter variance allows a value of a parameterized category to be converted to
a value from the same category with a different parameter. You can specify
*which direction* conversions are allowed for each parameter of a type category.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<span style='color:#898887;'>// Can convert #x to a parent type, e.g., Reader&lt;Child&gt; -&gt; Reader&lt;Parent&gt;.</span>
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Reader</span></b><span style='color:#c02040;'>&lt;</span><span style='color:#c04040;'>|</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {}

<span style='color:#898887;'>// Can convert #x to a child type, e.g., Writer&lt;Parent&gt; -&gt; Writer&lt;Child&gt;.</span>
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Writer</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c04040;'>|</span><span style='color:#c02040;'>&gt;</span> {}

<span style='color:#898887;'>// Can convert #x to a child type and #y to a parent type.</span>
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Function</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c04040;'>|</span><i><span style='color:#0057ae;'>#y</span></i><span style='color:#c02040;'>&gt;</span> {}

<span style='color:#898887;'>// Different possibilities for disallowing conversion of #x.</span>
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Type1</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {}
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Type2</span></b><span style='color:#c02040;'>&lt;</span><span style='color:#c04040;'>|</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c04040;'>|</span><span style='color:#c02040;'>&gt;</span> {}
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Type3</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#w</span></i><span style='color:#c04040;'>|</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c04040;'>|</span><i><span style='color:#0057ae;'>#y</span></i><span style='color:#c02040;'>&gt;</span> {}

<span style='color:#898887;'>// Multiple parameters are separated by &quot;,&quot;.</span>
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Type4</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#u</span></i>,<i><span style='color:#0057ae;'>#v</span></i><span style='color:#c04040;'>|</span><i><span style='color:#0057ae;'>#w</span></i>,<i><span style='color:#0057ae;'>#x</span></i><span style='color:#c04040;'>|</span><i><span style='color:#0057ae;'>#y</span></i>,<i><span style='color:#0057ae;'>#z</span></i><span style='color:#c02040;'>&gt;</span> {}</pre>

Type parameters for functions *cannot* specify variance.

When a parameter is either covariant (`<|#x>`) or contravariant (`<#x|>`),
there are certain additional limitations that apply to how it can be used within
a category:

- A covariant parameter cannot be used by callers to *write* with, e.g., as a
  function argument.
- A contravariant parameter cannot be used by callers to *read* with, e.g., as a
  function return.

These rules are applied recursively when a parameter is used in `refines`,
`defines`, and function arguments and returns.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Reader</span></b><span style='color:#c02040;'>&lt;</span><span style='color:#c04040;'>|</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  read () <b><span style='color:#006e28;'>-&gt;</span></b> (<b>optional</b> <i><span style='color:#0057ae;'>#x</span></i>)
}

<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Writer</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c04040;'>|</span><span style='color:#c02040;'>&gt;</span> {
  write (<i><span style='color:#0057ae;'>#x</span></i>) <b><span style='color:#006e28;'>-&gt;</span></b> ()
}

<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Queue</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#y</span></i><span style='color:#c02040;'>&gt;</span> {
  <span style='color:#898887;'>// Fine, since #y cannot vary.</span>
  <b>refines</b> <span style='color:#0057ae;'>Reader</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#y</span></i><span style='color:#c02040;'>&gt;</span>
  <b>refines</b> <span style='color:#0057ae;'>Writer</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#y</span></i><span style='color:#c02040;'>&gt;</span>
}

<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Data</span></b><span style='color:#c02040;'>&lt;</span><span style='color:#c04040;'>|</span><i><span style='color:#0057ae;'>#z</span></i><span style='color:#c02040;'>&gt;</span> {
  <span style='color:#898887;'>// NOT ALLOWED, since #z would be used for writing.</span>
  <span style='color:#898887;'>// refines Writer&lt;#z&gt;</span>

  <span style='color:#898887;'>// Fine, because #z is actually used for reading here, i.e., readAll allows</span>
  <span style='color:#898887;'>// the caller to read the data by passing it to the Writer.</span>
  readAll (<span style='color:#0057ae;'>Writer</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#z</span></i><span style='color:#c02040;'>&gt;</span>) <b><span style='color:#006e28;'>-&gt;</span></b> ()

  <span style='color:#898887;'>// NOT ALLOWED, since #z would be used for writing, i.e., the caller is</span>
  <span style='color:#898887;'>// writing data by passing it inside of a Reader.</span>
  <span style='color:#898887;'>// writeAll (Reader&lt;#z&gt;) -&gt; ()</span>
}</pre>

Also see the discussion of covariance and contravariance
[on Wikipedia][cov-con].

### Parameter Filters

Type parameters don't have access to any information about the type being
substituted unless filtering is applied.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>concrete</b> <b><span style='color:#0057ae;'>Data</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  <i><span style='color:#0057ae;'>#x</span></i> <b>requires</b> <i><span style='color:#0057ae;'>Formatted</span></i>  <span style='color:#898887;'>// Built-in @value interface.</span>

  <span style='color:#644a9b;'>@type</span> create (<i><span style='color:#0057ae;'>#x</span></i>) <b><span style='color:#006e28;'>-&gt;</span></b> (<span style='color:#0057ae;'>Data</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span>)
  <span style='color:#644a9b;'>@value</span> format () <b><span style='color:#006e28;'>-&gt;</span></b> (<i><span style='color:#0057ae;'>String</span></i>)
}

<b>define</b> <b><span style='color:#0057ae;'>Data</span></b> {
  <span style='color:#644a9b;'>@value</span> <i><span style='color:#0057ae;'>#x</span></i> value

  create (v) {
    <b>return</b> <span style='color:#0057ae;'>Data</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span>{ v }
  }

  format () {
    <b>return</b> value.formatted()
  }
}</pre>

In the above example, `formatted` is a function from `Formatted`, and can be
used because the filter `#x requires Formatted` prevents type substitution if
the type doesn't refine `Formatted`.

The reverse is also possible; you can require that a parameter be *assignable*
from a certain type. (This is arguably less useful, but mostly exists for
completeness.)

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>concrete</b> <b><span style='color:#0057ae;'>Data</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  <i><span style='color:#0057ae;'>#x</span></i> <b>allows</b> <i><span style='color:#0057ae;'>String</span></i>

  <span style='color:#644a9b;'>@type</span> create (<b>optional</b> <i><span style='color:#0057ae;'>#x</span></i>) <b><span style='color:#006e28;'>-&gt;</span></b> (<span style='color:#0057ae;'>Data</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span>)
  <span style='color:#644a9b;'>@value</span> orDefault () <b><span style='color:#006e28;'>-&gt;</span></b> (<i><span style='color:#0057ae;'>#x</span></i>)
}

<b>define</b> <b><span style='color:#0057ae;'>Data</span></b> {
  <span style='color:#644a9b;'>@value</span> <b>optional</b> <i><span style='color:#0057ae;'>#x</span></i> value

  create (v) {
    <b>return</b> <span style='color:#0057ae;'>Data</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span>{ v }
  }

  orDefault () {
    <b>if</b> (<b>present</b>(value)) {
      <b>return</b> <b>require</b>(value)
    } <b>else</b> {
      <b>return</b> <span style='color:#bf0303;'>&quot;Not Found&quot;</span>
    }
  }
}</pre>

Both can also be applied to other parameters:

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>concrete</b> <b><span style='color:#0057ae;'>Type</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i>,<i><span style='color:#0057ae;'>#y</span></i><span style='color:#c02040;'>&gt;</span> {
  <i><span style='color:#0057ae;'>#x</span></i> <b>requires</b> <i><span style='color:#0057ae;'>#y</span></i>
}</pre>

Filters can also specify required `@type` interfaces.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>concrete</b> <b><span style='color:#0057ae;'>Data</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  <i><span style='color:#0057ae;'>#x</span></i> <b>defines</b> <i><span style='color:#0057ae;'>LessThan</span></i><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span>  <span style='color:#898887;'>// Built-in @type interface.</span>

  <span style='color:#644a9b;'>@type</span> create (<i><span style='color:#0057ae;'>#x</span></i>) <b><span style='color:#006e28;'>-&gt;</span></b> (<span style='color:#0057ae;'>Data</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span>)
  <span style='color:#644a9b;'>@value</span> replaceIfLessThan (<i><span style='color:#0057ae;'>#x</span></i>) <b><span style='color:#006e28;'>-&gt;</span></b> ()
}

<b>define</b> <b><span style='color:#0057ae;'>Data</span></b> {
  <span style='color:#644a9b;'>@value</span> <i><span style='color:#0057ae;'>#x</span></i> value

  create (v) {
    <b>return</b> <span style='color:#0057ae;'>Data</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span>{ v }
  }

  replaceIfLessThan (v) {
    <b>if</b> (<i><span style='color:#0057ae;'>#x</span></i><span style='color:#644a9b;'>$</span>lessThan(v,value)) {
      value <b><span style='color:#006e28;'>&lt;-</span></b> v
    }
  }
}</pre>

In this example, `#x$lessThan(...)` is a type-function call being made on `#x`.

### Type Reduction

There are no explicit type casts in Zeolite, but the `reduce` built-in provides
a sort of "lazy" type conversion. It can only perform a type conversion that
would be allowed at compile time, but the check is deferred until runtime, after
parameter substitution has occurred.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<span style='color:#644a9b;'>@value</span> maybeConvert&lt;<i><span style='color:#0057ae;'>#x</span></i>&gt; () <b><span style='color:#006e28;'>-&gt;</span></b> (<b>optional</b> <i><span style='color:#0057ae;'>#x</span></i>)
maybeConvert () {
  <span style='color:#0057ae;'>Value</span> value <b><span style='color:#006e28;'>&lt;-</span></b> getValue()
  <b>return</b> <b>reduce</b>&lt;<span style='color:#0057ae;'>Value</span>,<i><span style='color:#0057ae;'>#x</span></i>&gt;(value)
}</pre>

Here `reduce` checks if `Value` can *in general* be converted to whatever `#x`
is in this particular function call. If it can be, it returns `value` as an
`optional #x`. Importantly, the runtime type of `value` is *completely* ignored.

A reasonable intuition here is that, if the compiler would have allowed an
automatic conversion from `Value` to `#x` had `#x` been known at compile-time
then it will return `value` here. Otherwise, it will return `empty`.

This can also be useful for debugging code that has type parameters.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>define</b> <b><span style='color:#0057ae;'>Something</span></b> {
  complicated (x) {
    <span style='color:#006e28;'>~</span> debugMessage(<span style='color:#bf0303;'>&quot;start&quot;</span>,x)
    <span style='color:#898887;'>// ...</span>
  }

  <span style='color:#644a9b;'>@type</span> debugMessage (<i><span style='color:#0057ae;'>String</span></i>,<i><span style='color:#0057ae;'>#x</span></i>) <b><span style='color:#006e28;'>-&gt;</span></b> ()
  debugMessage (message,x) {
    <i><span style='color:#0057ae;'>Formatted</span></i> val <b><span style='color:#006e28;'>&lt;-</span></b> <b>typename</b>&lt;<i><span style='color:#0057ae;'>#x</span></i>&gt;()
    <b>scoped</b> {
      <b>optional</b> <i><span style='color:#0057ae;'>Formatted</span></i> f <b><span style='color:#006e28;'>&lt;-</span></b> <b>reduce</b>&lt;<i><span style='color:#0057ae;'>#x</span></i>,<i><span style='color:#0057ae;'>Formatted</span></i>&gt;(x)
    } <b>in</b> <b>if</b> (<b>present</b>(f)) {
      <span style='color:#898887;'>// Only set if #x -&gt; Formatted; otherwise, it stays as &quot;?&quot;.</span>
      val <b><span style='color:#006e28;'>&lt;-</span></b> <b>require</b>(f)
    }
    <span style='color:#006e28;'>~</span> <span style='color:#0057ae;'>LazyStream</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>Formatted</span></i><span style='color:#c02040;'>&gt;</span><span style='color:#644a9b;'>$</span>new()
        .append(<span style='color:#bf0303;'>&quot;Debug (&quot;</span>).append(val).append(<span style='color:#bf0303;'>&quot;): &quot;</span>).append(message).append(<span style='color:#bf0303;'>&quot;</span><span style='color:#924c9d;'>\n</span><span style='color:#bf0303;'>&quot;</span>)
        .writeTo(<span style='color:#0057ae;'>SimpleOutput</span><span style='color:#644a9b;'>$</span>stderr())
  }
}</pre>

In this example, `x` is `formatted` only if `#x` is something that converts to
`Formatted`, which might be sufficient for a simple test case, e.g.,
`Something<String>$complicated("test")`. (The fallback above is just the name of
the type assigned to the parameter.)

### Internal Types

In some situations, a category might want to hide a type parameter from
external view.

In C++ or Java, something like this would be achieved with an interface and a
derived class containing an additional parameter.

```c++
// C++! Not Zeolite.

struct Value {
  template<class T>
  static std::unique_ptr<Value> create(T v) {
    return std::unique_ptr<Value>(new TypedValue<T>(v));
  }

  virtual ~Value() = default;
};

template<class T>
struct TypedValue : public Value {
  TypedValue(T v) : value(v) {}

  T value;
};
```

This additional indirection is unnecessary in Zeolite because the compiler
doesn't perform full parameter substitution at compile time.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>concrete</b> <b><span style='color:#0057ae;'>Value</span></b> {
  <span style='color:#644a9b;'>@type</span> create&lt;<i><span style='color:#0057ae;'>#x</span></i>&gt;
    <i><span style='color:#0057ae;'>#x</span></i> <b>requires</b> <i><span style='color:#0057ae;'>Formatted</span></i>
  (<i><span style='color:#0057ae;'>#x</span></i>) <b><span style='color:#006e28;'>-&gt;</span></b> (<span style='color:#0057ae;'>Value</span>)
}

<b>define</b> <b><span style='color:#0057ae;'>Value</span></b> {
  <b>types</b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#y</span></i><span style='color:#c02040;'>&gt;</span> {
    <i><span style='color:#0057ae;'>#y</span></i> <b>requires</b> <i><span style='color:#0057ae;'>Formatted</span></i>
  }

  <span style='color:#644a9b;'>@value</span> <i><span style='color:#0057ae;'>#y</span></i> value

  create (v) {
    <b>return</b> <span style='color:#0057ae;'>Value</span>{ <b>types</b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span>, v }
  }
}</pre>

The `types` keyword specifies additional internal type parameters that are
*permanent* once the value is initialized. The `{}` following `types<#x>` can be
used to specify filters to be applied to the types.

## Other Features

### Unions and Intersections

Zeolite provides type-union and type-intersection meta-types. The types used in
these meta-types act as "criteria" for any object that will be stored in them.

- A value with an *intersection type* `[A&B]` can be assigned from something
  that is *both* `A` and `B`, and can be assigned to *either* an `A` or `B`.
  There is a special *empty intersection* named `any` that can be assigned from
  any value but cannot be assigned to any other type.

  Intersections can be useful for requiring multiple interfaces without creating
  a new category that refines all of those interfaces.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Reader</span></b> {}

<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Writer</span></b> {}

<b>concrete</b> <b><span style='color:#0057ae;'>Data</span></b> {
  <b>refines</b> <span style='color:#0057ae;'>Reader</span>
  <b>refines</b> <span style='color:#0057ae;'>Writer</span>
  <span style='color:#644a9b;'>@type</span> new () <b><span style='color:#006e28;'>-&gt;</span></b> (<span style='color:#0057ae;'>Data</span>)
}

<span style='color:#898887;'>// ...</span>

<b><span style='color:#006e28;'>[</span></b><span style='color:#0057ae;'>Reader</span><span style='color:#006e28;'>&amp;</span><span style='color:#0057ae;'>Writer</span><b><span style='color:#006e28;'>]</span></b> val <b><span style='color:#006e28;'>&lt;-</span></b> <span style='color:#0057ae;'>Data</span><span style='color:#644a9b;'>$</span>new()
<span style='color:#0057ae;'>Reader</span> val2 <b><span style='color:#006e28;'>&lt;-</span></b> val
<span style='color:#0057ae;'>Writer</span> val3 <b><span style='color:#006e28;'>&lt;-</span></b> val</pre>

- A value with a *union type* `[A|B]` can be assigned from *either* `A` or `B`,
  but can only be assigned to something that *both* `A` and `B` can be assigned
  to. There is a special *empty union* named `all` that cannot ever be assigned
  a value but that can be assigned to everything. (`empty` is actually of type
  `optional all`.)

  Unions can be useful for limiting the types that can be assigned to a more
  general variable.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
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

<b><span style='color:#006e28;'>[</span></b><span style='color:#0057ae;'>Newspaper</span><span style='color:#006e28;'>|</span><span style='color:#0057ae;'>Magazine</span><b><span style='color:#006e28;'>]</span></b> val <b><span style='color:#006e28;'>&lt;-</span></b> <span style='color:#0057ae;'>Newspaper</span><span style='color:#644a9b;'>$</span>new()
<span style='color:#0057ae;'>Printable</span> val2 <b><span style='color:#006e28;'>&lt;-</span></b> val</pre>

### Function Merging

Each category can only have one function with a given name. In some cases, it's
possible to merge functions with the same name inherited from different parent
interfaces, however.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Getter</span></b><span style='color:#c02040;'>&lt;</span><span style='color:#c04040;'>|</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  get () <b><span style='color:#006e28;'>-&gt;</span></b> (<i><span style='color:#0057ae;'>#x</span></i>)
}

<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Storage</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  get () <b><span style='color:#006e28;'>-&gt;</span></b> (<i><span style='color:#0057ae;'>#x</span></i>)
  set (<i><span style='color:#0057ae;'>#x</span></i>) <b><span style='color:#006e28;'>-&gt;</span></b> ()
}

<b>concrete</b> <b><span style='color:#0057ae;'>Data</span></b> {
  <b>refines</b> <span style='color:#0057ae;'>Getter</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>Formatted</span></i><span style='color:#c02040;'>&gt;</span>  <span style='color:#898887;'>// String -&gt; Formatted</span>
  <b>refines</b> <span style='color:#0057ae;'>Storage</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>String</span></i><span style='color:#c02040;'>&gt;</span>

  <span style='color:#898887;'>// get is inherited from both Getter and Storage. The two can be merged with</span>
  <span style='color:#898887;'>// an explicit override, as long as it is compatible with all other versions</span>
  <span style='color:#898887;'>// of get that were inherited.</span>
  <span style='color:#644a9b;'>@value</span> get () <b><span style='color:#006e28;'>-&gt;</span></b> (<i><span style='color:#0057ae;'>String</span></i>)
}</pre>

The *minimum* requirement for merging is that all versions of the function have
matching argument, return, and type-parameter counts, and are "compatible" when
pairing each of those by position. (Parameter filters determine the "type" of
each type parameter, and parameter names don't matter.)

## Future Things

### Function Objects

At some point, Zeolite will probably support passing functions as arguments.
There are three levels of support, which might not happen at the same time:

1. Passing named functions, bound to some context, i.e., a type instance for a
   `@type` function and a value for a `@value` function.
2. Locally defined lambda functions that will require ad hoc closures to capture
   the local variables that they depend on.
3. Partial application (currying) by binding values to some of the arguments.

The third level will likely not happen, since there is no obvious syntax for
doing so. (Unlike currying in Haskell.)

### Anonymous Structures

Maybe.

### Exceptions

Probably not, but Zeolite already has stack traces for crashes.

### Packages and Name Scoping

At some point Zeolite will need a package system, or some other way to scope
category names to avoid name clashes. This is largely a syntactic consideration,
but it also requires some changes at the binary level to avoid name clashes
when linking.

### Random Access Primitives

Random access as a language primitive (e.g., arrays in C++ and Java) probably
won't happen. Arrays are most useful when you need to allocate a fixed number of
values up front and then access positions using arithmetic operations.

Random access will instead probably be handled via a standard-library category
that doesn't have its own special syntax. This can provide equivalent
functionality without needing to worry about the details of allocation.

### Immutable Types

A value of an immutable type cannot be modified by any procedure after
initialization. This can be useful for concurrency, and to otherwise limit the
possible places a bug can occur in the code.

Zeolite will eventually have immutable categories, where immutability is
enforced for the entire category; not just for selected values or for specific
references to values. This should be an easy addition to the type system.

A related concept is C++ `const`, which is used to protect *specific instances*
from modification, either locally or by specific functions.

An *incorrect* intuition of `const` is that it makes the entire object appear to
be immutable. In order to achieve those semantics, the code author needs to
understand C++ best-practices enough to design a `class` that has an "immutable
view" (i.e., `const` member functions) that protects things that are accessible
via indirection.

For example, `std::vector` was designed such that a `const std::vector<X>`
contains `const X` values. Poor design (i.e., default behavior) would allow
callers to modify elements of a `const std::vector<X>`. On the other hand, a
`const std::unique_ptr<X>` provides access to a non-`const` `X`, which can be
counter-intuitive for new C++ users.

This can be confusing for the reader and writer, it can be inconsistently
applied, and it can require quite a bit of extra boilerplate to get it right.
It's therefore unlikely that Zeolite will also have a concept of an "immutable
view" of a value that can otherwise be modified.

## Other Discussions

### Inheritance and Overriding

Zeolite doesn't allow one category to inherit procedures from another category,
unlike classes in Java and C++.

On the other hand, there are three common use-cases where inheriting procedures
is useful:

1. Extending the functionality of a base category.
2. Implementing procedures that a base category can call.
3. Overriding some of the functionality of a base category.

All of these idioms are possible in Zeolite; they just can't be done using
inheritance.

For example, the base and derived categories can implement the same interface,
and the derived category can store the base as a data member. The derived
category can then delegate calls to that data member. (Delegation might have its
own special syntax in the future.)

Since Zeolite doesn't use constructors, you can often just return a *different*
type from a factory method. In languages with constructors, the default
expectation is that there is a way to obtain a new value for any type. In
Zeolite, the default is that there *isn't* a way to obtain a new value.

This difference in expectations can be useful if the purpose of inheritance
would have been to implement a function to be called by the base type.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<span style='color:#898887;'>// Some interface for threads.</span>
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Thread</span></b> {
  start () <b><span style='color:#006e28;'>-&gt;</span></b> ()
  join () <b><span style='color:#006e28;'>-&gt;</span></b> ()
}

<span style='color:#898887;'>// Some interface for runnable procedures.</span>
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Runnable</span></b> {
  run () <b><span style='color:#006e28;'>-&gt;</span></b> ()
}

<span style='color:#898887;'>// A Thread implementation that executes a procedure.</span>
<b>concrete</b> <b><span style='color:#0057ae;'>RunnableThread</span></b> {
  <b>refines</b> <span style='color:#0057ae;'>Thread</span>
  <span style='color:#644a9b;'>@type</span> create (<span style='color:#0057ae;'>Runnable</span>) <b><span style='color:#006e28;'>-&gt;</span></b> (<span style='color:#0057ae;'>Thread</span>)
}

<span style='color:#898887;'>// A procedure that uses RunnableThread internally.</span>
<b>concrete</b> <b><span style='color:#0057ae;'>MyThread</span></b> {
  <b>refines</b> <span style='color:#0057ae;'>Runnable</span>
  <span style='color:#644a9b;'>@type</span> new () <b><span style='color:#006e28;'>-&gt;</span></b> (<span style='color:#0057ae;'>Thread</span>)
}

<b>define</b> <b><span style='color:#0057ae;'>MyThread</span></b> {
  new () {
    <b>return</b> <span style='color:#0057ae;'>RunnableThread</span><span style='color:#644a9b;'>$</span>create(<span style='color:#0057ae;'>MyThread</span>{})
  }

  run () {
    <span style='color:#898887;'>// The procedure to run in the thread.</span>
  }
}</pre>

In this example, `MyThread$new()` doesn't actually return a value of type
`MyThread`, but that shouldn't matter to the caller; the caller just needs a
`Thread`. There is no expectation that a caller should be able to directly get a
value of type `MyThread`.

### Implicit Types

Zeolite currently requires that all types be made explicit in places where types
are actually needed, e.g., creating variables and passing type arguments to
parameterized functions and categories. In most other places, types are
implicit. For example, calling a function on a return (e.g., `call1().call2()`)
doesn't require explicitly specifying the first return type.

Some languages take implicit typing a step further, like `auto` in C++. The
rationale behind `auto` is that the compiler knows what the type is, so you
shouldn't need to verify it.

#### C++ `auto`

`auto` was long overdue in C++ when it arrived because it changed this

```c++
const typename std::map<std::string,std::list<X>>::const_iterator pos = index.find(name);
if (pos != index.end()) {
  for (typename std::list<X>::const_iterator x = pos->second.begin(); x != pos->second.end(); ++x) {
    // ... something with *x
  }
}
```

into this:

```c++
const auto pos = index.find(name);
if (pos != index.end()) {
  for (const auto& x : pos->second) {
    // ... something with x
  }
}
```

This is now a critical feature of C++; *however*, its usefulness is also a
reflection of how templates are implemented in C++.

C++ templates use compile-time
[duck typing](https://en.wikipedia.org/wiki/Duck_typing), which means that the
compiler will only check validity once it knows the actual types being
substituted in.

```c++
template<class T>
void add(T l, T r) {
  // What type is sum? For some substitutions of T, the result might *not* be T.
  // In fact, we don't even know if + is a member function called from l, or if
  // it's a global function that has 2 arguments.
  auto sum = l+r;
}
```

In other situations, C++ `auto` is simply used to cut down on typing when the
type is more or less obvious or irrelevant.

#### Implicit Types in Zeolite

Although implicit typing can make code easier to write *and* read, the aspects
of C++ that make `auto` useful are not applicable in Zeolite.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<span style='color:#644a9b;'>@type</span> <b>interface</b> <b><span style='color:#0057ae;'>Plus</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  plus (<i><span style='color:#0057ae;'>#x</span></i>,<i><span style='color:#0057ae;'>#x</span></i>) <b><span style='color:#006e28;'>-&gt;</span></b> (<i><span style='color:#0057ae;'>#x</span></i>)
}

<span style='color:#898887;'>// ...</span>

<span style='color:#644a9b;'>@type</span> add&lt;<i><span style='color:#0057ae;'>#x</span></i>&gt;
  <i><span style='color:#0057ae;'>#x</span></i> <b>defines</b> <span style='color:#0057ae;'>Plus</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span>
(<i><span style='color:#0057ae;'>#x</span></i>,<i><span style='color:#0057ae;'>#x</span></i>) <b><span style='color:#006e28;'>-&gt;</span></b> ()

add (l,r) {
  <span style='color:#898887;'>// We can't do anything with #x without a constraint, but the constraint also</span>
  <span style='color:#898887;'>// tells us the return type.</span>
  <i><span style='color:#0057ae;'>#x</span></i> sum <b><span style='color:#006e28;'>&lt;-</span></b> <i><span style='color:#0057ae;'>#x</span></i><span style='color:#644a9b;'>$</span>plus(l,r)
}</pre>

Automatic type conversions in Zeolite also ensure that explicitly-specified
types are more relevant in context. This is because you only need to specify
what type a value will be *used as*, rather than the type it *actually is*.

Additionally, allowing implicit types when creating variables would likely make
Zeolite *less* readable, due to the extensive amount of implicit type
conversions it supports.

Another point to consider is the experience of the programmer when their code is
actually *incorrect* or needs refactoring.

For example, in Go the use of implicit types can actually make refactoring a
procedure into smaller functions more difficult for the programmer. (Go uses
implicit typing for local variables and explicit typing for function arguments
and returns.)

### Type Inference

Zeolite currently does not infer type arguments in type-parameter substitution.

For example:

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>concrete</b> <b><span style='color:#0057ae;'>Process</span></b> {
  <span style='color:#644a9b;'>@type</span> process&lt;<i><span style='color:#0057ae;'>#x</span></i>&gt; (<i><span style='color:#0057ae;'>#x</span></i>) <b><span style='color:#006e28;'>-&gt;</span></b> ()
}

<span style='color:#898887;'>// ...</span>

<span style='color:#006e28;'>~</span> <span style='color:#0057ae;'>Process</span><span style='color:#644a9b;'>$</span>process&lt;<i><span style='color:#0057ae;'>String</span></i>&gt;(<span style='color:#bf0303;'>&quot;string&quot;</span>)</pre>

In this example, even though `"string"` can be inferred to have type `String`,
the call still must pass `<String>` to `process`.

This is much different from C++ and Java, both of which allow parameters to be
skipped when calling functions.

Zeolite will probably *not* have type inference for parameter substitution in
the future. There are a few reasons for this:

- Automatic type conversions make it harder for the compiler to come up with a
  "best" match in many situations, which also means that the *reader of the
  code* would have the same difficulty figuring out the type.
- Code in Zeolite can call functions on type parameters, and `reduce` can
  operate on them at runtime, which makes them more like function arguments and
  less like "type place-holders".
- Type parameters are not necessary in Zeolite in many situations where Java or
  C++ would require them. For example, it is common in C++ to use a template to
  accept an arbitrary container type, rather than having a common interface that
  expresses container operations.

In Java and C++, type parameters for functions are more of a declaration to the
compiler that the function relies on to-be-determined types. In both languages,
the number and order of the parameters is often arbitrary, and parameterized
functions are often combined with function overloading.

Explicit types are a way to tell the compiler and *readers of your code* what
you *meant* to do. This forces compile-time errors to be localized, which makes
them easier to investigate and understand. Again, considering what happens when
the code itself is *incorrect* is sometimes more important than streamlining
*correct* code.

Haskell (which is what Zeolite is written in) uses *extensive* type inference
that spans multiple function calls. This cleans up the code quite a bit, but
it's very common for the compiler to show a type error in a function other than
the one containing the mistake, while showing *no errors* in the function that
actually contains one.

## Conclusion

Please experiment with Zeolite and share your thoughts. Please also contact me
if you are interested in helping with development.

[clang]: https://clang.llvm.org/cxx_status.html
[cov-con]: https://en.wikipedia.org/wiki/Covariance_and_contravariance_%28computer_science%29
[example]: https://github.com/ta0kira/zeolite/tree/master/example/tree
[ghc]: https://www.haskell.org/ghc/
[git]: https://git-scm.com/
