# Zeolite Programming Language

Zeolite is a general-purpose, statically-typed programming language. It focuses
on data objects and their uses, while attempting to avoid pitfalls of
object-oriented programming.

## Hello World

It's the [any%](https://en.wiktionary.org/wiki/any%25) of programming.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>concrete</b> <b><span style='color:#0057ae;'>HelloWorld</span></b> {
  <b>defines</b> <span style='color:#0057ae;'>Runner</span>
}

<b>define</b> <b><span style='color:#0057ae;'>HelloWorld</span></b> {
  run () {
    <span style='color:#006e28;'>~</span> <span style='color:#0057ae;'>LazyStream</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>String</span></i><span style='color:#c02040;'>&gt;</span><span style='color:#644a9b;'>$</span>new().append(<span style='color:#bf0303;'>&quot;Hello World</span><span style='color:#924c9d;'>\n</span><span style='color:#bf0303;'>&quot;</span>).writeTo(<span style='color:#0057ae;'>SimpleOutput</span><span style='color:#644a9b;'>$</span>stderr())
  }
}</pre>

## Motivation

The basic units of a Zeolite program are *categories* of types, each having a
fixed number of *type parameters*.

For example:

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>concrete</b> <b><span style='color:#0057ae;'>Type</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  <span style='color:#898887;'>// ...</span>
}</pre>

The category `Type` has a single type parameter `#x`. Passing a type to `Type`
gives you a *type instance*, e.g., `Type<Value>`. Here `Type` can be thought of
as a "function" that turns one type (`Value`) into another type (`Type<Value>`),
where `#x` is the name of the "type argument".

### Parameter Variance - Java

This is all somewhat standard until you consider how two type-instances of
`Type` relate to each other.

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
  `List` is useless.
- In `readItems`, the use of `extends` means that all *write* functionality in
  the `List` is useless.
- Each user of `List` determines how the type parameter is going to be used,
  rather than `List` specifying how it can be used.

The theme here is that the Java `List` focuses on appearing as a container of
objects, and each user chooses a way to make use of that container.

### Parameter Variance - Zeolite

Zeolite takes a different approach by forcing the type category to declare how
each type parameter can vary.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Writer</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c04040;'>|</span><span style='color:#c02040;'>&gt;</span> {
  append (<i><span style='color:#0057ae;'>#x</span></i>) <b><span style='color:#006e28;'>-&gt;</span></b> ()
}

<span style='color:#644a9b;'>@value</span> <b>interface</b> <b><span style='color:#0057ae;'>Reader</span></b><span style='color:#c02040;'>&lt;</span><span style='color:#c04040;'>|</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
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

In this example, the `|` to the *right* of `#x` in `Writer` means that any
`Writer` can be used as a `Writer` of a *more specific* type of object. This is
like `super` in the Java example, but it's permanent for all users.

Similarly, the `|` to the *left* of `#x` in `Reader` means that any `Reader` can
be used as a `Reader` of any *more general* type of object. This is similar to
`extends` in the Java example, but it's permanent for all users.

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
additional syntax aside from `|` where the parameter is defined.

## Language Overview

Despite the complex motivation behind Zeolite, programs written with it can be
simple and readable.

This is partly due to limitations on how types can be defined and used. The
theme of these limitations (listed below) is that the program must focus on how
types and values *can be used*, rather than on *the data they represent*.

The following list can be thought of as *things that are missing* when comparing
to other languages, which can be a good thing, once you get used to it.

- Each type category is either *concrete* or an *interface*. Concrete categories
  *cannot* be further extended, and interfaces *cannot* define procedural code.
  This means that the inheritance graph of types primarily consists of
  interfaces, with all procedural code living at the very bottom.
- All data members have *internal* visibility. This is more restrictive than
  `private` visibility in Java and C++ in that objects *cannot* directly access
  the data members of other objects of the same type.
- Function overloading (i.e., multiple functions with the same name) is not
  allowed; a category can only have one function with a given name. (*But*
  compatible functions with the same name from multiple parents can be merged.)
- Values cannot be missing (like `null` in Java) unless a specific qualifier is
  used for the variable, argument, or return.
- There are no constructors and there is no default initialization; values can
  only be created from factory functions.

Zeolite also has a limited meta-type system that operates on types themselves:

- A special type of interface called a *type interface* can be used for
  requiring type-level functions. This allows procedures to call functions on
  type parameters themselves.
- Constraints on type parameters can be used when declaring categories or
  functions, to ensure that the parameter can be used in a certain manner. This
  is similar to how Java allows syntax like `<X extends Parent>`, but it is done
  out of line so that constraints can contain more complex relationships between
  parameters.

## Basic Ideas

This section provides a language intro, starting with simple topics and getting
more advanced.

### Concrete Types, and Procedures

If you learn nothing else here, you at least need to be able to write something
that will execute. *Concrete* categories are the only place you can define
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
- `@value` means that what follows is specific to *allocated values* from the
  category. (Like instances in Java and C++.)
- The function `func` takes no arguments and returns nothing.
- `@type` means that what follows applies to *type instances*. In this case,
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
are compatible with the originals. Overridding can be done in either the
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

### Parameter Filters

Type parameters provide no information about the type being substituted unless
filtering is applied.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>concrete</b> <b><span style='color:#0057ae;'>Data</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  <i><span style='color:#0057ae;'>#x</span></i> <b>requires</b> <i><span style='color:#0057ae;'>Formatted</span></i>  <span style='color:#898887;'>// Built-in @value interface.</span>

  <span style='color:#644a9b;'>@type</span> create (<i><span style='color:#0057ae;'>#x</span></i>) <b><span style='color:#006e28;'>-&gt;</span></b> (<span style='color:#0057ae;'>Data</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span>)
  <span style='color:#644a9b;'>@value</span> format () <b><span style='color:#006e28;'>-&gt;</span></b> (<i><span style='color:#0057ae;'>String</span></i>)
}

<b>define</b> <b><span style='color:#0057ae;'>Data</span></b> {
  <span style='color:#644a9b;'>@value</span> <i><span style='color:#0057ae;'>#x</span></i> value

  create (v) {
    <b>return</b> <span style='color:#0057ae;'>Data</span>{ v }
  }

  format () {
    <b>return</b> value.formatted()
  }
}</pre>

In the above example, `formatted` is a function from `Formatted`, and can be
used because the filter `#x requires Formatted` prevents type substitution if
`Formatted` is not available.

The reverse is also possible; you can require that a parameter be *assignable*
from a certain type.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>concrete</b> <b><span style='color:#0057ae;'>Data</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  <i><span style='color:#0057ae;'>#x</span></i> <b>allows</b> <i><span style='color:#0057ae;'>String</span></i>

  <span style='color:#644a9b;'>@type</span> create (<b>optional</b> <i><span style='color:#0057ae;'>#x</span></i>) <b><span style='color:#006e28;'>-&gt;</span></b> (<span style='color:#0057ae;'>Data</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span>)
  <span style='color:#644a9b;'>@value</span> orDefault () <b><span style='color:#006e28;'>-&gt;</span></b> (<i><span style='color:#0057ae;'>#x</span></i>)
}

<b>define</b> <b><span style='color:#0057ae;'>Data</span></b> {
  <span style='color:#644a9b;'>@value</span> <b>optional</b> <i><span style='color:#0057ae;'>#x</span></i> value

  create (v) {
    <b>return</b> <span style='color:#0057ae;'>Data</span>{ v }
  }

  orDefault () {
    <b>if</b> (<b>present</b>(value)) {
      <b>return</b> <b>require</b>(value)
    } <b>else</b> {
      <b>return</b> <span style='color:#bf0303;'>&quot;Not Found&quot;</span>
    }
  }
}</pre>

Filters can also specify required `@type` interfaces.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>concrete</b> <b><span style='color:#0057ae;'>Data</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  <i><span style='color:#0057ae;'>#x</span></i> <b>defines</b> <i><span style='color:#0057ae;'>LessThan</span></i>  <span style='color:#898887;'>// Built-in @type interface.</span>

  <span style='color:#644a9b;'>@type</span> create (<i><span style='color:#0057ae;'>#x</span></i>) <b><span style='color:#006e28;'>-&gt;</span></b> (<span style='color:#0057ae;'>Data</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span>)
  <span style='color:#644a9b;'>@value</span> replaceIfLessThan (<i><span style='color:#0057ae;'>#x</span></i>) <b><span style='color:#006e28;'>-&gt;</span></b> ()
}

<b>define</b> <b><span style='color:#0057ae;'>Data</span></b> {
  <span style='color:#644a9b;'>@value</span> <i><span style='color:#0057ae;'>#x</span></i> value

  create (v) {
    <b>return</b> <span style='color:#0057ae;'>Data</span>{ v }
  }

  replaceIfLessThan (v) {
    <b>if</b> (<i><span style='color:#0057ae;'>#x</span></i><span style='color:#644a9b;'>$</span>lessThan(v,value)) {
      value <b><span style='color:#006e28;'>&lt;-</span></b> v
    }
  }
}</pre>

In this example, `#x$lessThan(...)` is a type-function call being made on `#x`.

### Multiple Returns

Functions can return more than one value. This can either be done by enclosing
values in `{}` or by naming them.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>concrete</b> <b><span style='color:#0057ae;'>Something</span></b> {
  <span style='color:#644a9b;'>@type</span> func1 () <b><span style='color:#006e28;'>-&gt;</span></b> (<i><span style='color:#0057ae;'>Int</span></i>,<i><span style='color:#0057ae;'>Int</span></i>)
  <span style='color:#644a9b;'>@type</span> func2 () <b><span style='color:#006e28;'>-&gt;</span></b> (<i><span style='color:#0057ae;'>Int</span></i>,<i><span style='color:#0057ae;'>Int</span></i>)
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
  }
}</pre>

The choice depends on the situation:

- Unnamed returns are fine if it's easy to get all of the values in the same
  place at the same time, and it requires specifying all values at all of the
  return points.
- Named returns are helpful when the values are determined separately, and might
  otherwise require extra temporary variables. Explicit return statements are
  disallowed other than `return _` to return with the current assignments.

There are also a few options for *recieving* multiple returns:

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>concrete</b> <b><span style='color:#0057ae;'>Something</span></b> {
  <span style='color:#644a9b;'>@type</span> func1 () <b><span style='color:#006e28;'>-&gt;</span></b> (<i><span style='color:#0057ae;'>Int</span></i>,<i><span style='color:#0057ae;'>Int</span></i>)
}

<b>define</b> <b><span style='color:#0057ae;'>Something</span></b> {
  func1 () {
    <b>return</b> { <span style='color:#b08000;'>1</span>, <span style='color:#b08000;'>2</span> }
  }

  <span style='color:#644a9b;'>@type</span> func2 () <b><span style='color:#006e28;'>-&gt;</span></b> ()
  func2 () {
    <i><span style='color:#0057ae;'>Int</span></i> x <b><span style='color:#006e28;'>&lt;-</span></b> <span style='color:#b08000;'>0</span>
    { x, <i><span style='color:#0057ae;'>Int</span></i> y } <b><span style='color:#006e28;'>&lt;-</span></b> func1()
    { <b>_</b>, y } <b><span style='color:#006e28;'>&lt;-</span></b> func1()
    <b>_</b> <b><span style='color:#006e28;'>&lt;-</span></b> func1()
  }
}</pre>

### Control Flow

Zeolite provides `if`/`elif`/`else` and `while` constructs.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>if</b> (x) {
  <span style='color:#898887;'>// something</span>
} <b>elif</b> (y) {
  <span style='color:#898887;'>// something</span>
} <b>else</b> {
  <span style='color:#898887;'>// something</span>
}</pre>

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>while</b> (x &gt; <span style='color:#b08000;'>0</span>) {
  <span style='color:#898887;'>// something</span>
}</pre>

These can (and should) be combined with `scoped` statements discussed in the
following section

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
} <b>in</b> <span style='color:#006e28;'>~</span> callWithSum(x + y)
<span style='color:#898887;'>// x and y don't exist here</span></pre>

There is currently no `for` loop syntax, but one can be created using `scoped`
and `while`.

<pre style='color:#1f1c1b;background-color:#ffffff;'>
<b>scoped</b> {
  <i><span style='color:#0057ae;'>Int</span></i> i <b><span style='color:#006e28;'>&lt;-</span></b> <span style='color:#b08000;'>0</span>
  <i><span style='color:#0057ae;'>Int</span></i> limit <b><span style='color:#006e28;'>&lt;-</span></b> <span style='color:#b08000;'>10</span>
} <b>in</b> <b>while</b> (i &lt; limit) {
  <span style='color:#898887;'>// ...</span>
  i <b><span style='color:#006e28;'>&lt;-</span></b> i+<span style='color:#b08000;'>1</span>
}</pre>

`for` loops are not a built-in syntax because such loops are a very specialized
case of `while`, and can become ugly very quickly if additional flexibility is
required.

### Optional and Weak Values

In Java, all values can be `null`. In C++, all pointers can be `nullptr`. In
most cases, such values are invalid and must be checked for in "clean" code.

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
<b>concrete</b> <b><span style='color:#0057ae;'>Something</span></b><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>#x</span></i><span style='color:#c02040;'>&gt;</span> {
  <span style='color:#644a9b;'>@type</span> complicated (<i><span style='color:#0057ae;'>#x</span></i>) <b><span style='color:#006e28;'>-&gt;</span></b> ()
}

<b>define</b> <b><span style='color:#0057ae;'>Something</span></b> {
  complicated (x) {
    <span style='color:#006e28;'>~</span> debugMessage(<span style='color:#bf0303;'>&quot;start&quot;</span>,x)
    <span style='color:#898887;'>// ...</span>
  }

  <span style='color:#644a9b;'>@type</span> debugMessage (<i><span style='color:#0057ae;'>String</span></i>,<i><span style='color:#0057ae;'>#x</span></i>) <b><span style='color:#006e28;'>-&gt;</span></b> ()
  debugMessage (message,x) {
    <i><span style='color:#0057ae;'>String</span></i> val <b><span style='color:#006e28;'>&lt;-</span></b> <span style='color:#bf0303;'>&quot;?&quot;</span>
    <b>scoped</b> {
      <b>optional</b> <i><span style='color:#0057ae;'>Formatted</span></i> f <b><span style='color:#006e28;'>&lt;-</span></b> <b>reduce</b>&lt;<i><span style='color:#0057ae;'>#x</span></i>,<i><span style='color:#0057ae;'>Formatted</span></i>&gt;(x)
    } <b>in</b> <b>if</b> (<b>present</b>(f)) {
      val <b><span style='color:#006e28;'>&lt;-</span></b> <b>require</b>(f).formatted()
    }
    <span style='color:#006e28;'>~</span> <span style='color:#0057ae;'>LazyStream</span><span style='color:#c02040;'>&lt;</span><i><span style='color:#0057ae;'>String</span></i><span style='color:#c02040;'>&gt;</span><span style='color:#644a9b;'>$</span>new()
        .append(<span style='color:#bf0303;'>&quot;Debug (&quot;</span>)
        .append(val)
        .append(<span style='color:#bf0303;'>&quot;): &quot;</span>)
        .append(message)
        .append(<span style='color:#bf0303;'>&quot;</span><span style='color:#924c9d;'>\n</span><span style='color:#bf0303;'>&quot;</span>).writeTo(<span style='color:#0057ae;'>SimpleOutput</span><span style='color:#644a9b;'>$</span>stderr())
  }
}</pre>

In this example, `x` is `formatted` only if `#x` is something that converts to
`Formatted`, which might be sufficient for a simple test case, e.g., `Int`.

### Unions and Intersections

Zeolite provides type-union and type-intersection meta-types. Justifying their
existence is outside of the scope of this intro.

- A value with a *union type* `[A|B]` can be assigned from *either* `A` or `B`,
  but can only be assigned to something that *both* `A` and `B` can be assigned
  to. There is a special *empty union* named `all` that cannot ever be assigned
  a value but that can be assigned to everything. (`empty` is actually of type
  `optional all`.)

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

- A value with an *intersection type* `[A&B]` can be assigned from something
  that is *both* `A` and `B`, and can be assigned to *either* an `A` or `B`.
  There is a special *empty intersection* named `any` that can be assigned from
  any value but cannot be assigned to any other type.

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
<span style='color:#0057ae;'>Reader</span> val2 <b><span style='color:#006e28;'>&lt;-</span></b> val</pre>

