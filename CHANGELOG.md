# Changelog

`elm-street` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].
## Unreleased

## 0.2.0.0 - Mar 29, 2022

* Remove GHC 8.4.4 and 8.6.5 from CI / tested-with
* Add GHC 8.10.7, 9.0.2 and 9.2.2 to CI / tested-with
* Support Json.Decode.Value as primitive
* Add primitive to represent NonEmpty lists as (a, List a) on elm side
* Add overlapping instance for Elm String, to ensure that Haskell `String`s are represented as `String` on Elm side (not as `List Char`)

##  0.1.0.4 - Jan 28, 2020

* Bump prettyprinter upper bound to allow building with lts-17.0

##  0.1.0.3 - Jun 29, 2020

* Update to lts-16.2

##  0.1.0.2 — Sep 13, 2019

* [#89](https://github.com/holmusk/elm-street/issues/89):
  Regulate parenthesis on complicated types in encoder, decoder and type
  generation.

##  0.1.0.1 — Sep 6, 2019

* Fix newtype qualified import bug in `Types.elm` generated module.
* Allow `pretty-printer-1.3` version.

## 0.1.0.0 — Sep 6, 2019

* [#80](https://github.com/holmusk/elm-street/issues/80):
  **Important:** *All* encoders for constructors with fields now have `tag` due
  to aeson decoder on Haskell side.

  **Migration guide 1:** Rename fields that will have `tag` name on the Elm
  side.

  **Migration guide 2:** If you have manual `ToJSON` instances that communicate
  with Elm via generated decoders, you need to add `tag` field with the
  constructor name:

  ```haskell
  data User = User { ... }

  instance ToJSON User where
      toJSON = [ "tag" .= ("User" :: Text), ... ]
  ```

* [#71](https://github.com/holmusk/elm-street/issues/71):
  **Breaking change:** Remove **overlapping** instance for `String`.

  **Migration guide:** Use `Text` instead of `String`.

* [#70](https://github.com/holmusk/elm-street/issues/70):
  Use qualified imports of generated types and function in Elm generated files.
* [#74](https://github.com/holmusk/elm-street/issues/74):
  Fix unit type `typeRef` encoder and decoder printers.
* [#72](https://github.com/holmusk/elm-street/issues/72):
  Use consistent encoders and decoders for unary constructors.
* [#79](https://github.com/holmusk/elm-street/issues/79):
  Implement cross-language golden tests.
* [#76](https://github.com/holmusk/elm-street/issues/76):
  Support GHC-8.6.5. Use common stanzas.
* [#86](https://github.com/holmusk/elm-street/issues/86):
  Refactor `Elm.Print` module and split into multiple smaller modules.
* [#73](https://github.com/holmusk/elm-street/issues/73):
  Clarify the restriction with reserved words in documentation.
* [#90](https://github.com/Holmusk/elm-street/issues/90)
  Support converting 3-tuples.
* [#6](https://github.com/holmusk/elm-street/issues/6):
  Test generated Elm code on CI.

## 0.0.1 — Mar 29, 2019

* [#64](https://github.com/holmusk/elm-street/issues/64):
  Fix indentation for the generated enums.
* [#66](https://github.com/holmusk/elm-street/issues/66):
  Patch JSON encoders and decoders for sum types with a single field.

## 0.0.0

* Initially created.

[1]: https://pvp.haskell.org
[2]: https://github.com/Holmusk/elm-street/releases
