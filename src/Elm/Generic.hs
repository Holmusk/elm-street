{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}

{- | Generic conversion of Haskell data types to Elm types.
-}

module Elm.Generic
       ( Elm (..)
       ) where

import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import GHC.Generics ((:*:), (:+:), C1, Constructor (..), D1, Datatype (..), Generic (..), M1 (..),
                     Rec0, S1, Selector (..), U1)
import Type.Reflection (Typeable, typeRep)

import Elm.Ast (ElmAlias (..), ElmConstructor (..), ElmDefinition (..), ElmRecordField (..),
                ElmType (..), TypeName (..))

import qualified Data.Text as T
import qualified GHC.Generics as Generic (from)


{- | Typeclass that describes how Haskell data types are converted to Elm ones.
-}
class Elm a where
    toElmDefinition :: Proxy a -> ElmDefinition

    default toElmDefinition
        :: (Generic a, GenericElmDefinition (Rep a))
        => Proxy a
        -> ElmDefinition
    toElmDefinition _ = genericToElmDefinition
        $ Generic.from (error "Proxy for generic elm was evaluated" :: a)

----------------------------------------------------------------------------
-- Generic instances
----------------------------------------------------------------------------

{- | Generic typeclass to generate whole 'ElmDefinition'. It has only one
instance: for the first top-level metadata that contains metainformation about
data type like @data type name@. Then it collects all constructors of the data
type and decides what to generate.
-}
class GenericElmDefinition (f :: k -> Type) where
    genericToElmDefinition :: f a -> ElmDefinition

instance (Datatype d, GenericElmConstructors f) => GenericElmDefinition (D1 d f) where
    genericToElmDefinition datatype = case genericToElmConstructors (unM1 datatype) of
        c :| [] -> case toElmConstructor c of
            Left fields -> DefAlias $ ElmAlias typeName fields
            Right ctor  -> DefType $ ElmType typeName [] (ctor :| [])
        c :| cs -> case traverse (rightToMaybe . toElmConstructor) (c :| cs) of
            -- TODO: this should be error but dunno what to do here
            Nothing    -> DefType $ ElmType ("ERROR_" <> typeName) [] (ElmConstructor "ERROR" [] :| [])
            Just ctors -> DefType $ ElmType typeName [] ctors
      where
        typeName :: Text
        typeName = T.pack $ datatypeName datatype

rightToMaybe :: Either l r -> Maybe r
rightToMaybe = either (const Nothing) Just

{- | Intermediate data type to help with the conversion from Haskell
constructors to Elm AST. In Haskell constructor fields may have names but may
not have.
-}
data GenericConstructor = GenericConstructor
    { genericConstructorName   :: Text
    , genericConstructorFields :: [(TypeName, Maybe Text)]
    }

{- | Generic constructor can be in one of the three states:

1. No fields: enum constructor.
2. All fields have names: record constructor.
3. Not all fields have names: plain constructor.
-}
toElmConstructor :: GenericConstructor -> Either (NonEmpty ElmRecordField) ElmConstructor
toElmConstructor GenericConstructor{..} = case genericConstructorFields of
    []   -> Right $ ElmConstructor genericConstructorName []
    f:fs -> case traverse toRecordField (f :| fs) of
        Nothing     -> Right $ ElmConstructor genericConstructorName $ map fst genericConstructorFields
        Just fields -> Left fields
  where
    toRecordField :: (TypeName, Maybe Text) -> Maybe ElmRecordField
    toRecordField (typeName, maybeFieldName) = ElmRecordField typeName <$> maybeFieldName


{- | Typeclass to collect all constructors of the Haskell data type generically. -}
class GenericElmConstructors (f :: k -> Type) where
    genericToElmConstructors :: f a -> NonEmpty GenericConstructor

-- | If it's a sum type then just combine constructors
instance (GenericElmConstructors f, GenericElmConstructors g) => GenericElmConstructors (f :+: g) where
    genericToElmConstructors _ =
        genericToElmConstructors (error "'f :+:' is evaluated" :: f p)
     <> genericToElmConstructors (error "':+: g' is evaluated" :: g p)

-- | Create singleton list for case of a one constructor.
instance (Constructor c, GenericElmFields f) => GenericElmConstructors (C1 c f) where
    genericToElmConstructors constructor = pure $ GenericConstructor
        (T.pack $ conName constructor)
        (genericToElmFields $ unM1 constructor)

-- | Collect all fields when inside constructor.
class GenericElmFields (f :: k -> Type) where
    genericToElmFields :: f a -> [(TypeName, Maybe Text)]

-- | If multiple fields then just combine all results.
instance (GenericElmFields f, GenericElmFields g) => GenericElmFields (f :*: g) where
    genericToElmFields _ =
        genericToElmFields (error "'f :*:' is evaluated" :: f p)
     <> genericToElmFields (error "':*: g' is evaluated" :: g p)

-- | Constructor without fields.
instance GenericElmFields U1 where
    genericToElmFields _ = []

-- | Single constructor field.
instance (Selector s, Typeable a) => GenericElmFields (S1 s (Rec0 a)) where
    genericToElmFields selector = case selName selector of
        ""   -> [(fieldTypeName, Nothing)]
        name -> [(fieldTypeName, Just $ T.pack name)]
      where
        fieldTypeName :: TypeName
        fieldTypeName = TypeName $ T.pack $ show (typeRep @a)
