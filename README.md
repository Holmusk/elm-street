# elm-street

[![Hackage](https://img.shields.io/hackage/v/elm-street.svg)](https://hackage.haskell.org/package/elm-street)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)
[![Stackage Lts](http://stackage.org/package/elm-street/badge/lts)](http://stackage.org/lts/package/elm-street)
[![Stackage Nightly](http://stackage.org/package/elm-street/badge/nightly)](http://stackage.org/nightly/package/elm-street)

Crossing the road between Haskell and Elm.

## Elm-side preparations

`Elm-street` needs to have some elm packages preinstalled in the project. You can make it with the following command:

```shell
elm install elm/time
elm install elm/json
elm install NoRedInk/elm-json-decode-pipeline
elm install rtfeldman/elm-iso8601-date-strings
```

## elm-example

To run:
1. Build and execute the `generate-elm` binary:
   ```
   cabal new-run generate-elm
   ```
2. Go to the `elm-example` folder:
   ```
   cd elm-example
   ```
3. Run the project:
   ```
   elm-app start
   ```

## Examples

Below you can see some examples of how Haskell data type are converted to Elm
types with the `elm-street` library.

### Records

**Haskell**

```haskell
data User = User
    { userName :: Text
    , userAge  :: Int
    }
```

**Elm**

```elm
type alias User =
    { name : String
    , age  : Int
    }
```

### Enums

**Haskell**

```haskell
data RequestStatus
    = Approved
    | Rejected
    | Reviewing
    deriving (Show, Read, Eq, Ord, Enum, Bounded)
```

**Elm**

```elm
type RequestStatus
    = Approved
    | Rejected
    | Reviewing

showRequestStatus : RequestStatus -> String
showRequestStatus status = case status of
    Approved  -> "Approved"
    Rejected  -> "Rejected"
    Reviewing -> "Reviewing"

readRequestStatus : String -> Maybe RequestStatus
readRequestStatus status = case status of
    "Approved"  -> Just Approved
    "Rejected"  -> Just Rejected
    "Reviewing" -> Just Reviewing
    _           -> Nothing

universeRequestStatus : List RequestStatus
universeRequestStatus = [Approved, Rejected, Reviewing]
```

### Newtypes

**Haskell**

```haskell
newtype Size = Size { unSize :: Int }
```

**Elm**

```elm
type alias Size =
    { unSize : Int
    }
```

### Newtypes with phantom types

**Haskell**

```haskell
newtype Id a = Id { unId :: Text }
```

**Elm**

```elm
type Id a = Id String

unId :: Id a -> String
unId (Id id) = id
```

### Sum types

**Haskell**

```haskell
data User
    = Regular Text Int
    | Visitor Text
```

**Elm**

```elm
type User
    = Regular String Int
    | Visitor String
```
