# A walk down elm-street

## The structure of the codebase

The `elm-street` codebase contains two separate programs. Here we 
will discuss the main one, located in the `Elm` folder in `src`. 
This program, which we will refer to as `elm-street` contains the 
logic for generating Elm modules from haskell types. The other 
program, located in the `backend` folder, is just a demo of 
`elm-street`, and hence not a dependency for it. For reference, here
is a generated dependency graph of the codebase.

## Abstract syntax tree of Elm types

We will study the codebase via traversing its dependency graph.
Thus we start with the `Elm.Ast` module. The term AST stands for
"Abstract Syntax Tree," which is a systematic way to represent
the set of possible expressions one may form. For instance, an
AST for elementary arithmetic could have integers as leaves and
allow one to combine any preexisting tree via either addition or
multiplication, thus associating any arithmetic expression like
`3+2*4` with a tree whose leavs are `3`,`2`, and `4`, and whose
binary nodes are `+` and `*`

In the case of `elm-street`, our expressions are types themselves
rather than the terms that contain them. Thus our leaves should
correspond to "atomic" or "primitive" types---i.e. those that 
cannot be broken down any further---and our nodes should correspond 
to ways of forming compound types for these atomic constituents.

To avoid confusion, we will refer to the Type declarations in the
`elm-street` modules as 'Types', with a capital 'T', and the Elm 
types we want to generate as 'types'. `ElmDefinition`, the first 
Type declared in this module specifies the possible nodes in the 
Elm type AST. In particular, it specifies that an elm type is one
of the following three possibilities.
 + an alias for a record type, `ElmAlias`
 + an algebraic datatype, `ElmType`
 + a primitive type, `ElmPrim`

Since an `ElmAlias` is merely a record type, it merely consists of
a name and a list (which the library stipulates be nonempty) of
field, which we store as text, with their corresponding types, which
we store as the `Text` newtype `TypeName`. This is sufficient to define
an Elm type alias, but we lastly include a boolean flag to indicate
weather this came from a newtype on the haskell side.

An `ElmType` is a bit more complicated. One needs to separately
consider both type constructors and data constructors. For the former
there is only one constructor, and we intuitively think of this
as the "name" of the type. The constructor then takes a list of type
variables. Also, just as above, we provide a newtype boolean flag.
The data constructor side is more interesting. Since Elm supports
algebraic data type, the set of type expressions should itself form 
a tree. The library makes a simplifying assumption that the ADT is 
presented in *normal form*, i.e. as a sum of products. Up to 
isomorphism, there is no loss of generality in this restriction. Thus
the data is represented as a non-empty list of data constructors,
each consisting of a text name and a `TypeRef`, which refers to
either a custom defined type's name or Elm's native name for a
primitive type.

Finally, the module defines the Type `ElmPrim` of primitive Elm types. 
We will not go down the list here, but we note that Haskell, unlike 
Elm, supports higher-kinded types, which means that an entity like 
`List`, which itself is not a type but rather a sort of type-level
"function" that takes a type as an argument and returns another type,
is a first class citizen and hence can be defined on its own without
argument. We also have that, unlike in Haskell, Elm does not identify
strings with lists of characters, and hence we include Elm strings as
a primitive type. Finally, for convenience, we compactify the common
compound types for JSON value and for time as primitive.

## Printing Elm type declarations

We now move up along the dependency graph to the `Print` folder. 
In this guide we will narrow focus to just printing Elm types 
and ignore encoders and decoders. Hence we will explore the 
`Elm.Print.Types` module. The `Elm.Print.Common` module simply
contains some text-based utilities such as wrapping text in 
parentheses, and hence we will not discuss it further.

Firstly, for all Elm type we wish to generate, we of course want
to create a type declaration. But for some special types, we can
also generate other useful fragments of code. In particular, if
we have an Enum type, i.e. an ADT whose data constructors are all
nullary, then we can auto-generate `show` and `read` functons,
along with a `universe` function that lists all of the constructors.
Also, for newtypes, we can generate an unwrapping function.

Rendering an `ElmDefinition` on a page corresponds to a function
mapping it to `Text`. To define this function, we use an intermediary
document type `Doc ann` from the `Prettyprint` library. Since this
type has a `Show` instance we can compose with show and convert 
to `Text`. Next we need to map the `ElmDefinition` AST to a `Doc ann`.
We have separate functions for type aliases and ADT's, but note that
we send primitives to the empty document! This is because we'll
never need to generate primitives by themselves. We will however need
to print `TypeRef`s which does require the names of primitive types
for one of its summands. These are defined as one would expect.

To print record aliases and algebraic data types, we choose a format
for each. For record aliases this is
```Elm
type alias TypeName = 
  { field1 : Type1 
  , field2 : Type2 
  }
```
while for ADT's we follow 
```Elm
type TypeConstructor typeVar =
  DataConstructor2 SomeFixedType typeVar
  DataConstructor1 typeVar
  DataConstructor0
```
The `Prettyprinter` library allows one to combine a list of `Doc ann`s,
vertically separated. Hence in both cases, the first line begins with 
the names of the types or type constructors. All subsequent lines are
rendered systematically, with appropriate care taken to spacing, symbols,
and parentheses.

## Generating Elm type modules

We next jump to `Elm.Generate`. We will not focus on the highly 
technical `Elm.Generic`. The central thing we must know about it
is that it both declares and instantiates a type class `Elm`, 
which equips a Haskell type `a` with a method `toElmDefinition` 
that maps a type to `ElmDefinition`. In `Elm.Generate`, we define 
a new typeclass, `RenderElm`, which uses this method to produce
the source code text for a type (and encoder/decoder) declaration.

The central function in this module is `generateElm`, which given
some settings---consisting of file paths in which to place the newly
generated Elm modules---performs the `IO` action of writing the
generated source code for a set of `RenderElm` constrained types, 
with appended headers, to some correctly placed `.elm` files. 