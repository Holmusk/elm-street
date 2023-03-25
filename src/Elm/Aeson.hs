{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Options used to derive FromJSON/ToJSON instance. These options generally
comply to @elm-street@ rules regarding names.
-}

module Elm.Aeson
       ( elmStreetParseJson
       , elmStreetParseJsonWith
       , elmStreetToJson
       , elmStreetToJsonWith
       , elmStreetJsonOptions

       , ElmStreet (..)
       ) where

import Data.Aeson (FromJSON (..), GFromJSON, GToJSON, Options (..), ToJSON (..), Value, Zero,
                   defaultOptions, genericParseJSON, genericToJSON)
import Data.Aeson.Types (Parser)
import GHC.Generics (Generic, Rep)
import Type.Reflection (Typeable)

import Elm.Generic (Elm (..), CodeGenOptions (..), GenericElmDefinition (..), ElmStreetGenericConstraints, defaultCodeGenOptions)

import qualified Data.Text as T
import qualified GHC.Generics as Generic (from)


{- | Allows to create 'Data.Aeson.FromJSON' instance for data types supported by 
@elm-street@. Strips data type name prefix from every field.

__Example:__

The following @JSON@

@
{ \"name\": \"John\"
, \"age\": 42
}
@

is decoded in the following way for each of the specified types:

+-------------------------------+--------------------------+
| Haskell data type             | Parsed type              |
+===============================+==========================+
| @                             | @                        |
| data User = User              | User                     |
|    { userName :: String       |    { userName = \"John\" |
|    , userAge  :: Int          |    , userAge  = 42       |
|    }                          |    }                     |
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
>>> decode @User "{\"age\":42,\"name\":\"John\",\"tag\":\"User\"}"
Just (User {userName = "John", userAge = 42})


>>> data VeryLongType = VeryLongType { vltName :: String, vltAge :: Int } deriving (Generic, Show)
>>> instance FromJSON VeryLongType where parseJSON = elmStreetParseJson
>>> decode @VeryLongType "{\"age\":42,\"name\":\"John\",\"tag\":\"VeryLongType\"}"
Just (VeryLongType {vltName = "John", vltAge = 42})

-}
elmStreetParseJson
    :: forall a .
       (Typeable a, Generic a, GFromJSON Zero (Rep a))
    => Value
    -> Parser a
elmStreetParseJson = elmStreetParseJsonWith (defaultCodeGenOptions @a)

{- | Use custom 'CodeGenOptions' to customize the behavior of derived FromJSON instance.
-}
elmStreetParseJsonWith
    :: forall a .
       (Generic a, GFromJSON Zero (Rep a))
    => CodeGenOptions
    -> Value
    -> Parser a
elmStreetParseJsonWith options = genericParseJSON (elmStreetJsonOptions options)

{- | Allows to create 'Data.Aeson.ToJSON' instance for types supported by @elm-street@.
Strips type name prefix from every record field.

>>> data User = User { userName :: String, userAge :: Int } deriving (Generic, Show)
>>> instance ToJSON User where toJSON = elmStreetToJson
>>> encode $ User { userName = "John", userAge = 42 }
"{\"age\":42,\"name\":\"John\",\"tag\":\"User\"}"

>>> data VeryLongType = VeryLongType { vltName :: String, vltAge :: Int } deriving (Generic, Show)
>>> instance ToJSON VeryLongType where toJSON = elmStreetToJson
>>> encode $ VeryLongType {vltName = "John", vltAge = 42}
"{\"age\":42,\"name\":\"John\",\"tag\":\"VeryLongType\"}"

>>> data User = User { name :: String, age :: Int } deriving (Generic, Show)
>>> instance ToJSON User where toJSON = elmStreetToJson
>>> encode $ User { name = "John", age = 42 }
"{\"age\":42,\"name\":\"John\",\"tag\":\"User\"}"
-}
elmStreetToJson
    :: forall a .
       (Typeable a, Generic a, GToJSON Zero (Rep a))
    => a
    -> Value
elmStreetToJson = elmStreetToJsonWith (defaultCodeGenOptions @a)

{- | Use custom 'CodeGenOptions' to customize the behavior of derived ToJSON instance.
-}
elmStreetToJsonWith
    :: forall a .
       (Generic a, GToJSON Zero (Rep a))
    => CodeGenOptions
    -> a
    -> Value
elmStreetToJsonWith options = genericToJSON (elmStreetJsonOptions options)

-- | Build @elm-street@ compatible 'Data.Aeson.Options' from 'CodeGenOptions'.
elmStreetJsonOptions :: CodeGenOptions -> Options
elmStreetJsonOptions options = defaultOptions
    { fieldLabelModifier = T.unpack . cgoFieldLabelModifier options . T.pack
    , tagSingleConstructors = True
    }

{- | Newtype for reusing in @DerivingVia@.

In order to use it with your type @MyType@ add the following deriving to your type:

@
    __deriving__ (Elm, ToJSON, FromJSON) __via__ ElmStreet MyType
@
-}
newtype ElmStreet a = ElmStreet
    { unElmStreet :: a
    }

instance (ElmStreetGenericConstraints a, Typeable a) => Elm (ElmStreet a) where
    toElmDefinition _ = genericToElmDefinition (defaultCodeGenOptions @a)
        $ Generic.from (error "Proxy for generic elm was evaluated" :: a)

instance (Typeable a, Generic a, GToJSON Zero (Rep a)) => ToJSON (ElmStreet a) where
    toJSON = elmStreetToJson . unElmStreet

instance (Typeable a, Generic a, GFromJSON Zero (Rep a)) => FromJSON (ElmStreet a) where
    parseJSON = fmap ElmStreet . elmStreetParseJson
