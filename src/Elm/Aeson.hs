{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Options used to derive FromJSON/ToJSON instance. These options generally
comply to @elm-street@ rules regarding names.
-}

module Elm.Aeson
       ( elmStreetParseJson
       , elmStreetToJson
       , elmStreetJsonOptions

       , ElmStreet (..)
       ) where

import Data.Aeson (FromJSON (..), GFromJSON, GToJSON, Options (..), ToJSON (..), Value, Zero,
                   defaultOptions, genericParseJSON, genericToJSON)
import Data.Aeson.Types (Parser)
import GHC.Generics (Generic, Rep)
import Type.Reflection (Typeable, typeRep)

import Elm.Ast (TypeName (..))
import Elm.Generic (Elm (..), GenericElmDefinition (..), HasLessThanEightUnnamedFields,
                    HasNoNamedSum, HasNoTypeVars, stripTypeNamePrefix)

import qualified Data.Text as T
import qualified GHC.Generics as Generic (from)


{- | Allows to create 'Data.Aeson.FromJSON' instance that strips the supported
by @elm-street@ data type name prefix from every field..

__Example:__

With the following @JSON@

@
{ \"name\": \"John\"
, \"age\": 42
}
@

it is decoded it the following way for each of the specified types:

+-------------------------------+--------------------------+
| Haskell data type             | Parsed type              |
+===============================+==========================+
| @                             | @                        |
| data User = User              | User                     |
| \   { userName :: String      |    { userName = \"John\" |
| \   , userAge  :: Int         |    , userAge  = 42       |
| \   }                         |    }                     |
| @                             | @                        |
+-------------------------------+--------------------------+
|                               |                          |
| @                             | @                        |
| data LongUser = LongUser      | LongUser                 |
|    { luName :: String         |    { luName = \"John\"   |
|    , luAge  :: Int            |    , luAge  = 42         |
|    }                          |    }                     |
| @                             | @                        |
+-------------------------------+--------------------------+
| @                             | @                        |
| data SimpleUser = SimpleUser  | SimpleUser               |
|    { name :: String           |    { name = \"John\"     |
|    , age  :: Int              |    , age  = 42           |
|    }                          |    }                     |
| @                             | @                        |
+-------------------------------+--------------------------+

>>> data User = User { userName :: String, userAge :: Int } deriving (Generic, Show)
>>> instance FromJSON User where parseJSON = elmStreetParseJson
>>> decode @User "{ \"name\": \"John\", \"age\": 42 }"
Just (User {userName = "John", userAge = 42})


>>> data VeryLongType = VeryLongType { vltName :: String, vltAge :: Int } deriving (Generic, Show)
>>> instance FromJSON VeryLongType where parseJSON = elmStreetParseJson
>>> decode @VeryLongType "{ \"name\": \"John\", \"age\": 42 }"
Just (VeryLongType {vltName = "John", vltAge = 42})

-}
elmStreetParseJson
    :: forall a .
       (Typeable a, Generic a, GFromJSON Zero (Rep a))
    => Value
    -> Parser a
elmStreetParseJson = genericParseJSON (elmStreetJsonOptions @a)

{- | Allows to create 'Data.Aeson.ToJSON' instance that strips the supported by
@elm-street@ data type name prefix from every field.

>>> data User = User { userName :: String, userAge :: Int } deriving (Generic, Show)
>>> instance ToJSON User where toJSON = elmStreetToJson
>>> encode $ User { userName = "John", userAge = 42 }
"{\"age\":42,\"name\":\"John\"}"

>>> data VeryLongType = VeryLongType { vltName :: String, vltAge :: Int } deriving (Generic, Show)
>>> instance ToJSON VeryLongType where toJSON = elmStreetToJson
>>> encode $ VeryLongType {vltName = "John", vltAge = 42}
"{\"age\":42,\"name\":\"John\"}"

>>> data User = User { name :: String, age :: Int } deriving (Generic, Show)
>>> instance ToJSON User where toJSON = elmStreetToJson
>>> encode $ User { name = "John", age = 42 }
"{\"age\":42,\"name\":\"John\"}"
-}
elmStreetToJson
    :: forall a .
       (Typeable a, Generic a, GToJSON Zero (Rep a))
    => a
    -> Value
elmStreetToJson = genericToJSON (elmStreetJsonOptions @a)

{- | Options to strip type name from the field names.

+----------------+----------------+---------------------+
| Data type name | Field name     | Stripped field name |
+================+================+=====================+
| @User@         | @userName@     | @name@              |
+----------------+----------------+---------------------+
| @AaaBbbCcc@    | @abcFieldName@ | @fieldName@         |
+----------------+----------------+---------------------+
| @Foo@          | @field@        | @field@             |
+----------------+----------------+---------------------+
| @Field@        | @field@        | @field@             |
+----------------+----------------+---------------------+

-}
elmStreetJsonOptions :: forall a . Typeable a => Options
elmStreetJsonOptions = defaultOptions
    { fieldLabelModifier = T.unpack . stripTypeNamePrefix typeName . T.pack
    }
  where
    typeName :: TypeName
    typeName = TypeName $ T.pack $ show $ typeRep @a


{- | Newtype for reusing in @DerivingVia@.

In order to use it with your type @MyType@ add the following deriving to your type:

@
    deriving (Elm, ToJSON, FromJSON) via ElmStreet MyType
@
-}
newtype ElmStreet a = ElmStreet
    { unElmStreet :: a
    }

instance ( HasNoTypeVars a
         , HasLessThanEightUnnamedFields a
         , HasNoNamedSum a
         , Generic a
         , GenericElmDefinition (Rep a)
         ) => Elm (ElmStreet a) where
    toElmDefinition _ = genericToElmDefinition
        $ Generic.from (error "Proxy for generic elm was evaluated" :: a)

instance (Typeable a, Generic a, GToJSON Zero (Rep a)) => ToJSON (ElmStreet a) where
    toJSON = elmStreetToJson . unElmStreet

instance (Typeable a, Generic a, GFromJSON Zero (Rep a)) => FromJSON (ElmStreet a) where
    parseJSON = fmap ElmStreet . elmStreetParseJson
