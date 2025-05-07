# elm-street

![logo](https://holmusk.dev/images/projects/elm_street.png)

[![Hackage](https://img.shields.io/hackage/v/elm-street.svg)](https://hackage.haskell.org/package/elm-street)
[![MPL-2.0 license](https://img.shields.io/badge/license-MPL--2.0-blue.svg)](LICENSE)

Crossing the road between Haskell and Elm.

## What is this library about?

`elm-street` allows you to automatically generate definitions of Elm data types and compatible JSON encoders and decoders
 from Haskell types. This helps to avoid writing and maintaining huge chunk of boilerplate code when developing full-stack
applications.

## Getting started

In order to use `elm-street` features, you need to perform the following steps:

1. Add `elm-street` to the dependencies of your Haskell package.
2. Derive the `Elm` typeclass for relevant data types. You also need to derive
   JSON instances according to `elm-street` naming scheme.
   This can be done like this:
   ```haskell
   import Elm (Elm, elmStreetParseJson, elmStreetToJson)

   data User = User
       { userName :: Text
       , userAge  :: Int
       } deriving (Generic)
         deriving anyclass (Elm)

   instance ToJSON   User where toJSON = elmStreetToJson
   instance FromJSON User where parseJSON = elmStreetParseJson
   ```
   > **NOTE:** This requires extensions `-XDerivingStrategies`, `-XDeriveGeneric`, `-XDeriveAnyClass`.

   Alternatively you can use `-XDerivingVia` to remove some boilerplate (available since GHC 8.6.1):
   ```haskell
   import Elm (Elm, ElmStreet (..))

   data User = User
       { userName :: Text
       , userAge  :: Int
       } deriving (Generic)
         deriving (Elm, ToJSON, FromJSON) via ElmStreet User
   ```
3. Create list of all types you want to expose to Elm:
   ```haskell
   type Types =
      '[ User
       , Status
       ]
   ```
   > **NOTE:** This requires extension `-XDataKinds`.
4. Use `generateElm` function to output definitions to specified directory under
   specified module prefix.
   ```haskell
   main :: IO ()
   main = generateElm @Types $ defaultSettings "frontend/src" ["Core", "Generated"]
   ```
   > **NOTE:** This requires extension `-XTypeApplications`.

   When executed, the above program generates the following files:

     + `frontend/src/Core/Generated/Types.elm`: `Core.Generated.Types` module with the definitions for the types, as well as show*, read*, un*, and universe* functions as specified in [src/Elm/Print/Types.hs](./src/Elm/Print/Types.hs)
     + `frontend/src/Core/Generated/Encoder.elm`: `Core.Generated.Encoder` module with the JSON encoders for the types
     + `frontend/src/Core/Generated/Decoder.elm`: `Core.Generated.Decoder` module with the JSON decoders for the types
     + `frontend/src/Core/Generated/ElmStreet.elm`: `Core.Generated.ElmStreet` module with bundled helper functions

## Elm-side preparations

If you want to use capabilities provided by `elm-street` in your Elm
application, you need to have several Elm packages preinstalled in the project. You
can install them with the following commands:

```shell
elm install elm/time
elm install elm/json
elm install NoRedInk/elm-json-decode-pipeline
elm install rtfeldman/elm-iso8601-date-strings
```

## Library restrictions

`Elm-street` is **not** trying to be as general as possible and support every
use-case. The library is opinionated in some decisions and contains several
limitations, specifically:

1. Record fields must be prefixed with the type name or its abbreviation.
   ```haskell
   data UserStatus = UserStatus
       { userStatusId      :: Id
       , userStatusRemarks :: Text
       }

   data HealthReading = HealthReading
       { hrUser   :: User
       , hrDate   :: UTCTime
       , hrWeight :: Double
       }
   ```
2. Data types with type variables are not supported (see [issue #45](https://github.com/Holmusk/elm-street/issues/45) for more details).
   Though, if type variables are phantom, you can still implement `Elm` instance which
   will generate valid Elm defintions. Here is how you can create `Elm` instance for
   `newtype`s with phantom type variables:
   ```haskell
   newtype Id a = Id { unId :: Text }

   instance Elm (Id a) where
       toElmDefinition _ = elmNewtype @Text "Id" "unId"
   ```
3. Sum types with records are not supported (because it's a bad practice to have
   records in sum types).
   ```haskell
   -- - Not supported
   data Address
       = Post { postCode :: Text }
       | Full { fullStreet :: Text, fullHouse :: Int }

   ```
4. Sum types with more than 8 fields in at least one constructor are not
   supported.
   ```haskell
   -- - Not supported
   data Foo
       = Bar Int Text
       | Baz Int Int Text Text Double Double Bool Bool Char
   ```
5. Records with fields that reference the type itself are not supported. This
   limitation is due to the fact that `elm-street` generates `type alias` for
   record data type. So the generated Elm type for the following Haskell data
   type won't compile in Elm:
   ```haskell
   data User = User
       { userName      :: Text
       , userFollowers :: [User]
       }
   ```
6. Generated JSON encoders and decoders are consistent with default behavior of
   derived `ToJSON/FromJSON` instances from the `aeson` library except you need
   to strip record field prefixes. Fortunately, this also can be done
   generically. You can use functions from `Elm.Aeson` module to derive `JSON`
   instances from the `aeson` package.
7. Only `UTCTime` Haskell data type is supported and it's translated to `Posix`
   type in Elm.
8. Some words in Elm are considered reserved and naming a record field with one of these words (prefixed with the type name, see 1) will result in the generated Elm files to not compile. So, the following words should not be used as field names:
   * `if`
   * `then`
   * `else`
   * `case`
   * `of`
   * `let`
   * `in`
   * `type`
   * `module`
   * `where`
   * `import`
   * `exposing`
   * `as`
   * `port`
   * `tag` (reserved for constructor name due to `aeson` options)
9. For newtypes `FromJSON` and `ToJSON` instances should be derived using `newtype` strategy. And `Elm` should be derived using `anyclass` strategy:
   ```haskell
   newtype Newtype = Newtype Int
       deriving newtype (FromJSON, ToJSON)
       deriving anyclass (Elm)
   ```

## Play with frontend example

The `frontend` directory contains example of minimal Elm project that shows how
generated types are used. To play with this project, do:

1. Build and execute the `generate-elm` binary:
   ```
   cabal new-run generate-elm
   ```
2. Run Haskell backend:
   ```
   cabal new-run run-backend
   ```
3. In separate terminal tab go to the `frontend` folder:
   ```
   cd frontend
   ```
4. Run the frontend:
   ```
   elm-app start
   ```

## Generated examples

Below you can see some examples of how Haskell data types are converted to Elm
types with JSON encoders and decoders using the `elm-street` library.

### Records

**Haskell**

```haskell
data User = User
    { userName :: Text
    , userAge  :: Int
    } deriving (Generic)
      deriving (Elm, ToJSON, FromJSON) via ElmStreet User
```

**Elm**

```elm
type alias User =
    { name : String
    , age : Int
    }

encodeUser : User -> Value
encodeUser x = E.object
    [ ("name", E.string x.name)
    , ("age", E.int x.age)
    ]

decodeUser : Decoder User
decodeUser = D.succeed User
    |> required "name" D.string
    |> required "age" D.int
```

### Enums

**Haskell**

```haskell
data RequestStatus
    = Approved
    | Rejected
    | Reviewing
    deriving (Generic)
    deriving (Elm, ToJSON, FromJSON) via ElmStreet RequestStatus
```

**Elm**

```elm
type RequestStatus
    = Approved
    | Rejected
    | Reviewing

showRequestStatus : RequestStatus -> String
showRequestStatus x = case x of
    Approved -> "Approved"
    Rejected -> "Rejected"
    Reviewing -> "Reviewing"

readRequestStatus : String -> Maybe RequestStatus
readRequestStatus x = case x of
    "Approved" -> Just Approved
    "Rejected" -> Just Rejected
    "Reviewing" -> Just Reviewing
    _ -> Nothing

universeRequestStatus : List RequestStatus
universeRequestStatus = [Approved, Rejected, Reviewing]

encodeRequestStatus : RequestStatus -> Value
encodeRequestStatus = E.string << showRequestStatus

decodeRequestStatus : Decoder RequestStatus
decodeRequestStatus = elmStreetDecodeEnum readRequestStatus
```

### Newtypes

**Haskell**

```haskell
newtype Age = Age
    { unAge :: Int
    } deriving (Generic)
      deriving newtype (FromJSON, ToJSON)
      deriving anyclass (Elm)
```

**Elm**

```elm
type alias Age =
    { age : Int
    }

encodeAge : Age -> Value
encodeAge x = E.int x.age

decodeAge : Decoder Age
decodeAge = D.map Age D.int
```

### Newtypes with phantom types

**Haskell**

```haskell
newtype Id a = Id
    { unId :: Text
    } deriving (Generic)
      deriving newtype (FromJSON, ToJSON)

instance Elm (Id a) where
    toElmDefinition _ = elmNewtype @Text "Id" "unId"
```

**Elm**

```elm
type alias Id =
    { unId : String
    }

encodeId : Id -> Value
encodeId x = E.string x.unId

decodeId : Decoder Id
decodeId = D.map Id D.string
```

### Sum types

**Haskell**

```haskell
data Guest
    = Regular Text Int
    | Visitor Text
    | Blocked
    deriving (Generic)
    deriving (Elm, ToJSON, FromJSON) via ElmStreet Guest
```

**Elm**

```elm
type Guest
    = Regular String Int
    | Visitor String
    | Blocked

encodeGuest : Guest -> Value
encodeGuest x = E.object <| case x of
    Regular x1 x2 -> [("tag", E.string "Regular"), ("contents", E.list identity [E.string x1, E.int x2])]
    Visitor x1 -> [("tag", E.string "Visitor"), ("contents", E.string x1)]
    Blocked  -> [("tag", E.string "Blocked"), ("contents", E.list identity [])]

decodeGuest : Decoder Guest
decodeGuest =
    let decide : String -> Decoder Guest
        decide x = case x of
            "Regular" -> D.field "contents" <| D.map2 Regular (D.index 0 D.string) (D.index 1 D.int)
            "Visitor" -> D.field "contents" <| D.map Visitor D.string
            "Blocked" -> D.succeed Blocked
            c -> D.fail <| "Guest doesn't have such constructor: " ++ c
    in D.andThen decide (D.field "tag" D.string)
```
