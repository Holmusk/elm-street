{-# OPTIONS_GHC -fno-warn-redundant-constraints #-}

{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Generic conversion of Haskell data types to Elm types.
-}

module Elm.Generic
       ( -- * Main data type for the user
         Elm (..)
       , TElm
       , elmTypeRef
       , elmRef

         -- * Generic utilities
       , GenericElmDefinition (..)
       , GenericElmConstructors (..)
       , GenericElmFields (..)

       , GenericConstructor (..)
       , toElmConstructor

         -- * Internals
       , HasNoTypeVars
       , TypeVarsError
       , stripTypeNamePrefix
       ) where

import Data.Char (isLower, toLower)
import Data.Int (Int16, Int32, Int8)
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.Void (Void)
import Data.Word (Word16, Word32, Word8)
import GHC.Generics ((:*:), (:+:), C1, Constructor (..), D1, Datatype (..), Generic (..), M1 (..),
                     Rec0, S1, Selector (..), U1)
import GHC.TypeLits (ErrorMessage (..), Nat, TypeError)
import Type.Reflection (Typeable, typeRep)

import Elm.Ast (ElmAlias (..), ElmConstructor (..), ElmDefinition (..), ElmPrim (..),
                ElmRecordField (..), ElmType (..), TypeName (..), TypeRef (..), definitionToRef)

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT (Text)
import qualified GHC.Generics as Generic (from)


{- | Typeclass that describes how Haskell data types are converted to Elm ones.
-}
class Elm a where
    toElmDefinition :: Proxy a -> ElmDefinition

    default toElmDefinition
        :: (HasNoTypeVars a, Generic a, GenericElmDefinition (Rep a))
        => Proxy a
        -> ElmDefinition
    toElmDefinition _ = genericToElmDefinition
        $ Generic.from (error "Proxy for generic elm was evaluated" :: a)

-- | 'TypeRef' for the existing type.
elmTypeRef :: forall a . Elm a => TypeRef
elmTypeRef = definitionToRef $ toElmDefinition (Proxy @a)

{- | Like 'elmTypeRef' but uses type name display from Haskell to display custom
data types. This is required to show type variables.
-}
elmRef :: forall a . TElm a => TypeRef
elmRef = case elmTypeRef @a of
    prim@(RefPrim _) -> prim
    RefCustom _      -> RefCustom $ TypeName $ T.pack $ show $ typeRep @a

-- | Constraint alias for things that have both 'Typeable' and 'Elm' constraints.
type TElm a = (Typeable a, Elm a)

----------------------------------------------------------------------------
-- Primitive instances
----------------------------------------------------------------------------

instance Elm ()   where toElmDefinition _ = DefPrim ElmUnit
instance Elm Void where toElmDefinition _ = DefPrim ElmNever
instance Elm Bool where toElmDefinition _ = DefPrim ElmBool
instance Elm Char where toElmDefinition _ = DefPrim ElmChar

instance Elm Int    where toElmDefinition _ = DefPrim ElmInt
instance Elm Int8   where toElmDefinition _ = DefPrim ElmInt
instance Elm Int16  where toElmDefinition _ = DefPrim ElmInt
instance Elm Int32  where toElmDefinition _ = DefPrim ElmInt
instance Elm Word   where toElmDefinition _ = DefPrim ElmInt
instance Elm Word8  where toElmDefinition _ = DefPrim ElmInt
instance Elm Word16 where toElmDefinition _ = DefPrim ElmInt
instance Elm Word32 where toElmDefinition _ = DefPrim ElmInt

instance Elm Float  where toElmDefinition _ = DefPrim ElmFloat
instance Elm Double where toElmDefinition _ = DefPrim ElmFloat

instance {-# OVERLAPPING #-} Elm String where toElmDefinition _ = DefPrim ElmString

instance Elm Text    where toElmDefinition _ = DefPrim ElmString
instance Elm LT.Text where toElmDefinition _ = DefPrim ElmString

-- TODO: should it be 'Bytes' from @bytes@ package?
-- https://package.elm-lang.org/packages/elm/bytes/latest/Bytes
-- instance Elm B.ByteString  where toElmDefinition _ = DefPrim ElmString
-- instance Elm LB.ByteString where toElmDefinition _ = DefPrim ElmString

instance Elm UTCTime where toElmDefinition _ = DefPrim ElmTime

instance TElm a => Elm (Maybe a) where
    toElmDefinition _ = DefPrim $ ElmMaybe $ elmRef @a

instance (TElm a, TElm b) => Elm (Either a b) where
    toElmDefinition _ = DefPrim $ ElmResult (elmRef @a) (elmRef @b)

instance (TElm a, TElm b) => Elm (a, b) where
    toElmDefinition _ = DefPrim $ ElmPair (elmRef @a) (elmRef @b)

instance TElm a => Elm [a] where
    toElmDefinition _ = DefPrim $ ElmList (elmRef @a)

instance TElm a => Elm (NonEmpty a) where
    toElmDefinition _ = DefPrim $ ElmList (elmRef @a)

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
    genericToElmDefinition datatype = case genericToElmConstructors (TypeName typeName) (unM1 datatype) of
        c :| [] -> case toElmConstructor c of
            Left fields -> DefAlias $ ElmAlias typeName fields elmIsNewtype
            Right ctor  -> DefType $ ElmType typeName [] elmIsNewtype (ctor :| [])
        c :| cs -> case traverse (rightToMaybe . toElmConstructor) (c :| cs) of
            -- TODO: this should be error but dunno what to do here
            Nothing    -> DefType $ ElmType ("ERROR_" <> typeName) [] False (ElmConstructor "ERROR" [] :| [])
            Just ctors -> DefType $ ElmType typeName [] elmIsNewtype ctors
      where
        typeName :: Text
        typeName = T.pack $ datatypeName datatype

        elmIsNewtype :: Bool
        elmIsNewtype = isNewtype datatype

rightToMaybe :: Either l r -> Maybe r
rightToMaybe = either (const Nothing) Just

{- | Intermediate data type to help with the conversion from Haskell
constructors to Elm AST. In Haskell constructor fields may have names but may
not have.
-}
data GenericConstructor = GenericConstructor
    { genericConstructorName   :: !Text
    , genericConstructorFields :: ![(TypeRef, Maybe Text)]
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
    toRecordField :: (TypeRef, Maybe Text) -> Maybe ElmRecordField
    toRecordField (typeRef, maybeFieldName) = ElmRecordField typeRef <$> maybeFieldName


{- | Typeclass to collect all constructors of the Haskell data type generically. -}
class GenericElmConstructors (f :: k -> Type) where
    genericToElmConstructors
        :: TypeName  -- ^ Name of the data type; to be stripped
        -> f a  -- ^ Generic value
        -> NonEmpty GenericConstructor  -- ^ List of the data type constructors

-- | If it's a sum type then just combine constructors
instance (GenericElmConstructors f, GenericElmConstructors g) => GenericElmConstructors (f :+: g) where
    genericToElmConstructors name _ =
        genericToElmConstructors name (error "'f :+:' is evaluated" :: f p)
     <> genericToElmConstructors name (error "':+: g' is evaluated" :: g p)

-- | Create singleton list for case of a one constructor.
instance (Constructor c, GenericElmFields f) => GenericElmConstructors (C1 c f) where
    genericToElmConstructors name constructor = pure $ GenericConstructor
        (T.pack $ conName constructor)
        (genericToElmFields name $ unM1 constructor)

-- | Collect all fields when inside constructor.
class GenericElmFields (f :: k -> Type) where
    genericToElmFields
        :: TypeName  -- ^ Name of the data type; to be stripped
        -> f a  -- ^ Generic value
        -> [(TypeRef, Maybe Text)]

-- | If multiple fields then just combine all results.
instance (GenericElmFields f, GenericElmFields g) => GenericElmFields (f :*: g) where
    genericToElmFields name _ =
        genericToElmFields name (error "'f :*:' is evaluated" :: f p)
     <> genericToElmFields name (error "':*: g' is evaluated" :: g p)

-- | Constructor without fields.
instance GenericElmFields U1 where
    genericToElmFields _ _ = []

-- | Single constructor field.
instance (Selector s, TElm a) => GenericElmFields (S1 s (Rec0 a)) where
    genericToElmFields typeName selector = case selName selector of
        ""   -> [(elmRef @a, Nothing)]
        name -> [(elmRef @a, Just $ stripTypeNamePrefix typeName $ T.pack name)]

{- | Strips name of the type name from field name prefix.

>>> stripTypeNamePrefix (TypeName "User") "userName"
"name"

>>> stripTypeNamePrefix (TypeName "HealthReading") "healthReadingId"
"id"

>>> stripTypeNamePrefix (TypeName "RecordUpdate") "ruRows"
"rows"
-}
stripTypeNamePrefix :: TypeName -> Text -> Text
stripTypeNamePrefix (TypeName typeName) fieldName =
    case T.stripPrefix (headToLower typeName) fieldName of
        Just rest -> headToLower rest
        Nothing   -> headToLower $ T.dropWhile isLower fieldName
  where
    headToLower :: Text -> Text
    headToLower t = case T.uncons t of
        Nothing      -> error "Cannot use 'headToLower' on empty Text"
        Just (x, xs) -> T.cons (toLower x) xs

----------------------------------------------------------------------------
-- ~Magic~
----------------------------------------------------------------------------

{- | This type family checks whether data type has type variables and throws
custom compiler error if it has. Since there's no generic way to get all type
variables, current implementation is limited only to 6 variables. This looks
like a reasonable number.
-}
type family HasNoTypeVars (f :: k) :: Constraint where
    HasNoTypeVars (t a b c d e f) = TypeError (TypeVarsError t 6)
    HasNoTypeVars (t a b c d e)   = TypeError (TypeVarsError t 5)
    HasNoTypeVars (t a b c d)     = TypeError (TypeVarsError t 4)
    HasNoTypeVars (t a b c)       = TypeError (TypeVarsError t 3)
    HasNoTypeVars (t a b)         = TypeError (TypeVarsError t 2)
    HasNoTypeVars (t a)           = TypeError (TypeVarsError t 1)
    HasNoTypeVars t               = ()

type family TypeVarsError (t :: k) (n :: Nat) :: ErrorMessage where
    TypeVarsError t n =
             'Text "'elm-street' currently doesn't support Generic deriving of the 'Elm' typeclass"
        ':$$: 'Text "for data types with type variables. But '"
              ':<>: 'ShowType t ':<>: 'Text "' has " ':<>: 'ShowType n ':<>: 'Text " variables."
        ':$$: 'Text ""
        ':$$: 'Text "See the following issue for more details:"
        ':$$: 'Text "    * https://github.com/Holmusk/elm-street/issues/45"
        ':$$: 'Text ""
