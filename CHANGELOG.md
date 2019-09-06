# Changelog

`elm-street` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## Unreleased: 0.1.0.0

* [#80](https://github.com/holmusk/elm-street/issues/80):
  *All* encoders for constructors with fields now have `tag`
  due to aeson decoder on Haskell side.
* [#74](https://github.com/holmusk/elm-street/issues/74):
  Fix unit type `typeRef` encoder and decoder printers.
* [#72](https://github.com/holmusk/elm-street/issues/72):
  Use consistent encoders and decoders for unary constructors.
* [#71](https://github.com/holmusk/elm-street/issues/71):
  **Breaking change:** Remove **overlapping** instance for `String`.

  **Migration guide:** Use `Text` instead.
* [#70](https://github.com/holmusk/elm-street/issues/70):
  Use qualified imports of generated types and function in Elm generated files.
* [#76](https://github.com/holmusk/elm-street/issues/76):
  Support GHC-8.6.5. Use common stanzas.
* [#73](https://github.com/holmusk/elm-street/issues/73):
  Clarify the restriction with reserved words in documentation.
* [#90](https://github.com/Holmusk/elm-street/issues/90)
  Support converting 3-tuples

## 0.0.1 — Mar 29, 2019

* [#64](https://github.com/holmusk/elm-street/issues/64):
  Fix indentation for the generated enums.
* [#66](https://github.com/holmusk/elm-street/issues/66):
  Patch JSON encoders and decoders for sum types with a single field.

## 0.0.0

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/Holmusk/elm-street/releases
