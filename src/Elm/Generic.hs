{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DefaultSignatures    #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

{- | Generic conversion of Haskell data types to Elm types.
-}

module Elm.Generic
       ( -- * Main data type for the user
         Elm (..)
       , elmRef

         -- * Smart constructors
       , elmNewtype

         -- * Generic utilities
       , GenericElmDefinition (..)
       , GenericElmConstructors (..)
       , GenericElmFields (..)

       , GenericConstructor (..)
       , toElmConstructor
         -- * Customizing generated elm code and JSON instances
       , CodeGenOptions (..)
       , defaultCodeGenOptions

         -- * Type families for compile-time checks
       , HasNoTypeVars
       , TypeVarsError

       , HasLessThanEightUnnamedFields
       , FieldsError
       , CheckFields
       , Max

       , HasNoNamedSum
       , NamedSumError
       , CheckNamedSum
       , CheckConst
       , ElmStreetGenericConstraints

         -- * Internals
       , stripTypeNamePrefix
       ) where

import Data.Aeson (Value)
import Data.Char (isLower, toLower)
import Data.Int (Int16, Int32, Int8)
import Data.Kind (Constraint, Type)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Type.Reflection (Typeable, typeRep)
import Data.Time.Clock (UTCTime)
import Data.Type.Bool (If, type (||))
import Data.Void (Void)
import Data.Word (Word16, Word32, Word8)
import GHC.Generics (C1, Constructor (..), D1, Datatype (..), Generic (..), M1 (..), Meta (..),
                     Rec0, S1, Selector (..), U1, (:*:), (:+:))
import GHC.TypeLits (ErrorMessage (..), Nat, TypeError)
import GHC.TypeNats (type (+), type (<=?))

import Elm.Ast (ElmConstructor (..), ElmDefinition (..), ElmPrim (..), ElmRecord (..),
                ElmRecordField (..), ElmType (..), TypeName (..), TypeRef (..), definitionToRef)

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT (Text)
import qualified GHC.Generics as Generic (from)


{- | Typeclass that describes how Haskell data types are converted to Elm ones.
-}
class Elm a where
    toElmDefinition :: Proxy a -> ElmDefinition

    default toElmDefinition
        :: (ElmStreetGenericConstraints a, Typeable a)
        => Proxy a
        -> ElmDefinition
    toElmDefinition _ = genericToElmDefinition (defaultCodeGenOptions @a)
        $ Generic.from (error "Proxy for generic elm was evaluated" :: a)

{- | Returns 'TypeRef' for the existing type. This function always returns the
name of the type without any type variables added.
-}
elmRef :: forall a . Elm a => TypeRef
elmRef = definitionToRef $ toElmDefinition $ Proxy @a

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

instance Elm Text    where toElmDefinition _ = DefPrim ElmString
instance Elm LT.Text where toElmDefinition _ = DefPrim ElmString

instance Elm Value where toElmDefinition _ = DefPrim ElmValue

-- TODO: should it be 'Bytes' from @bytes@ package?
-- https://package.elm-lang.org/packages/elm/bytes/latest/Bytes
-- instance Elm B.ByteString  where toElmDefinition _ = DefPrim ElmString
-- instance Elm LB.ByteString where toElmDefinition _ = DefPrim ElmString

instance Elm UTCTime where toElmDefinition _ = DefPrim ElmTime

instance Elm a => Elm (Maybe a) where
    toElmDefinition _ = DefPrim $ ElmMaybe $ elmRef @a

instance (Elm a, Elm b) => Elm (Either a b) where
    toElmDefinition _ = DefPrim $ ElmResult (elmRef @a) (elmRef @b)

instance (Elm a, Elm b) => Elm (a, b) where
    toElmDefinition _ = DefPrim $ ElmPair (elmRef @a) (elmRef @b)

instance (Elm a, Elm b, Elm c) => Elm (a, b, c) where
    toElmDefinition _ = DefPrim $ ElmTriple (elmRef @a) (elmRef @b) (elmRef @c)

instance Elm a => Elm [a] where
    toElmDefinition _ = DefPrim $ ElmList (elmRef @a)

-- Overlapping instance to ensure that Haskell @String@ is represented as Elm @String@
-- and not as @List Char@ based based on @Elm a => Elm [a]@ instance
instance {-# OVERLAPPING #-} Elm String where
    toElmDefinition _ = DefPrim ElmString

instance Elm a => Elm (NonEmpty a) where
    toElmDefinition _ = DefPrim $ ElmNonEmptyPair (elmRef @a)

----------------------------------------------------------------------------
-- Smart constructors
----------------------------------------------------------------------------

{- | This function can be used to create manual 'Elm' instances easily for
@newtypes@ where 'Generic' deriving doesn't work. This function can be used like
this:

@
__newtype__ Id a = Id { unId :: Text }

__instance__ Elm (Id a) __where__
    toElmDefinition _ = elmNewtype @Text "Id" "unId"
@
-}
elmNewtype :: forall a . Elm a => Text -> Text -> ElmDefinition
elmNewtype typeName fieldName = DefRecord $ ElmRecord
    { elmRecordName      = typeName
    , elmRecordFields    = ElmRecordField (elmRef @a) fieldName :| []
    , elmRecordIsNewtype = True
    }

----------------------------------------------------------------------------
-- Generic instances
----------------------------------------------------------------------------

{- | Generic typeclass to generate whole 'ElmDefinition'. It has only one
instance: for the first top-level metadata that contains metainformation about
data type like @data type name@. Then it collects all constructors of the data
type and decides what to generate.
-}
class GenericElmDefinition (f :: k -> Type) where
    genericToElmDefinition :: CodeGenOptions -> f a -> ElmDefinition

instance (Datatype d, GenericElmConstructors f) => GenericElmDefinition (D1 d f) where
    genericToElmDefinition options datatype = case genericToElmConstructors options (unM1 datatype) of
        c :| [] -> case toElmConstructor c of
            Left fields -> DefRecord $ ElmRecord typeName fields elmIsNewtype
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
        :: CodeGenOptions 
        -> f a  -- ^ Generic value
        -> NonEmpty GenericConstructor  -- ^ List of the data type constructors

-- | If it's a sum type then just combine constructors
instance (GenericElmConstructors f, GenericElmConstructors g) => GenericElmConstructors (f :+: g) where
    genericToElmConstructors options _ =
        genericToElmConstructors options (error "'f :+:' is evaluated" :: f p)
     <> genericToElmConstructors options (error "':+: g' is evaluated" :: g p)

-- | Create singleton list for case of a one constructor.
instance (Constructor c, GenericElmFields f) => GenericElmConstructors (C1 c f) where
    genericToElmConstructors options constructor = pure $ GenericConstructor
        (T.pack $ conName constructor)
        (genericToElmFields options $ unM1 constructor)

-- | Collect all fields when inside constructor.
class GenericElmFields (f :: k -> Type) where
    genericToElmFields
        :: CodeGenOptions
        -> f a  -- ^ Generic value
        -> [(TypeRef, Maybe Text)]

-- | If multiple fields then just combine all results.
instance (GenericElmFields f, GenericElmFields g) => GenericElmFields (f :*: g) where
    genericToElmFields options _ =
        genericToElmFields options (error "'f :*:' is evaluated" :: f p)
     <> genericToElmFields options (error "':*: g' is evaluated" :: g p)

-- | Constructor without fields.
instance GenericElmFields U1 where
    genericToElmFields _ _ = []

-- | Single constructor field.
instance (Selector s, Elm a) => GenericElmFields (S1 s (Rec0 a)) where
    genericToElmFields options selector = case selName selector of
        ""   -> [(elmRef @a, Nothing)]
        name -> [(elmRef @a, Just $ cgoFieldLabelModifier options $ T.pack name)]

{- | Strips name of the type name from field name prefix.

>>> stripTypeNamePrefix (TypeName "User") "userName"
"name"

>>> stripTypeNamePrefix (TypeName "HealthReading") "healthReadingId"
"id"

>>> stripTypeNamePrefix (TypeName "RecordUpdate") "ruRows"
"rows"

>>> stripTypeNamePrefix (TypeName "Foo") "foo"
"foo"

>>> stripTypeNamePrefix (TypeName "Foo") "abc"
"abc"
-}
stripTypeNamePrefix :: TypeName -> Text -> Text
stripTypeNamePrefix (TypeName typeName) fieldName =
    case T.stripPrefix (headToLower typeName) fieldName of
        Just rest -> leaveIfEmpty rest
        Nothing   -> leaveIfEmpty (T.dropWhile isLower fieldName)
  where
    headToLower :: Text -> Text
    headToLower t = case T.uncons t of
        Nothing      -> error "Cannot use 'headToLower' on empty Text"
        Just (x, xs) -> T.cons (toLower x) xs

    -- if all lower case then leave field as it is
    leaveIfEmpty :: Text -> Text
    leaveIfEmpty rest = if T.null rest then fieldName else headToLower rest

{- | CodeGenOptions allow for customizing some aspects of generated Elm code as well as
 ToJSON and FromJSON instances derived generically.

They can be passed to 'elmStreetParseJsonWith', 'elmStreetToJsonWith' and 'genericToElmDefinition'
to influence the behavior of FromJSON \/ ToJSON and Elm instances respectively.

Note that for Generated Elm encoders \/ decoders to be compatible
with ToJSON \/ FromJSON instances for given type,
__the same CodeGenOptions must be used in Elm \/ ToJSON \/ FromJSON instance declarations__.

Example: Say you don't like the default behavior (stripping type name prefix from all record fields)
and you would like to keep all record field names unmodified instead.
You can achieve that by declaring custom options:

@
myCodeGenOptions :: CodeGenOptions
myCodeGenOptions = CodeGenOptions { cgoFieldLabelModifier = id }
@

And then pass these options when defining Elm \/ ToJSON \/ FromJSON instances.
It is recommended to use DerivingVia to reduce the amount of boilerplate needed.
First declare a newtype whose Elm \/ ToJSON \/ FromJSON instances use your custom CodeGenOptions:

@
newtype CustomElm a = CustomElm {unCustomElm :: a}

instance ElmStreetGenericConstraints a => Elm (CustomElm a) where
    toElmDefinition _ = genericToElmDefinition myCodeGenOptions $
        GHC.Generics.from (error "Proxy for generic elm was evaluated" :: a)

instance (Generic a, GToJSON Zero (Rep a)) => ToJSON (CustomElm a) where
    toJSON = elmStreetToJsonWith myCodeGenOptions . unCustomElm

instance (Generic a, GFromJSON Zero (Rep a)) => FromJSON (CustomElm a) where
    parseJSON = fmap CustomElm . elmStreetParseJsonWith myCodeGenOptions
@

Then derive Elm \/ ToJSON \/ FromJSON instance via that newtype:

@
data MyType = MyType
    { myTypeFieldOne :: String
    , myTypeFieldTwo :: Int
    } deriving stock (Show, Generic)
      deriving (Elm, ToJSON, FromJSON) via CustomElm MyType
@

We can check that type name prefix is no longer stripped from record field names:

>>> encode (MyType "Hello" 10)
"{\"myTypeFieldOne\":\"Hello\",\"myTypeFieldTwo\":10,\"tag\":\"MyType\"}"
-}
newtype CodeGenOptions = CodeGenOptions
    { cgoFieldLabelModifier :: Text -> Text -- ^ Function that modifies record field names (e.g. by dropping type name prefix)
    }

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
defaultCodeGenOptions :: forall a. Typeable a => CodeGenOptions
defaultCodeGenOptions = CodeGenOptions (stripTypeNamePrefix typeName)
  where
    typeName :: TypeName
    typeName = TypeName $ T.pack $ show $ typeRep @a

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

{- | This type family checks whether each constructor of the sum data type has
less than eight unnamed fields and throws custom compiler error if it has.
-}
type family HasLessThanEightUnnamedFields (f :: k) :: Constraint where
    HasLessThanEightUnnamedFields t =
        If (CheckFields (Rep t) <=? 8)
            (() :: Constraint)
            (TypeError (FieldsError t))

type family CheckFields (f :: k -> Type) :: Nat where
    CheckFields (D1 _ f) = CheckFields f
    CheckFields (f :+: g) = Max (CheckFields f) (CheckFields g)
    CheckFields (C1 _ f) = CheckFields f
    CheckFields (f :*: g) = CheckFields f + CheckFields g
    CheckFields (S1 ('MetaSel ('Just _ ) _ _ _) _) = 0
    CheckFields (S1 _ _) = 1
    CheckFields _ = 0

type family Max (x :: Nat) (y :: Nat) :: Nat where
    Max x y = If (x <=? y) y x

type family FieldsError (t :: k) :: ErrorMessage where
    FieldsError t =
             'Text "'elm-street' doesn't support Constructors with more than 8 unnamed fields."
        ':$$: 'Text "But '" ':<>: 'ShowType t ':<>: 'Text "' has more."

{- | This type family checks whether each constructor of the sum data type has
less than eight unnamed fields and throws custom compiler error if it has.
-}
type family HasNoNamedSum (f :: k) :: Constraint where
    HasNoNamedSum t =
        If (CheckNamedSum (Rep t))
            (TypeError (NamedSumError t))
            (() :: Constraint)

-- | Is the data type id Sum type with named fields?
type family CheckNamedSum (f :: k -> Type) :: Bool where
    CheckNamedSum (D1 _ f) = CheckNamedSum f
    CheckNamedSum (f :+: g) = CheckConst f || CheckConst g
    CheckNamedSum _ = 'False

-- | Check if Sum type has named fields at least for one of the Constructors.
type family CheckConst (f :: k -> Type) :: Bool where
    CheckConst (f :+: g) = CheckConst f || CheckConst g
    CheckConst (C1 _ f) = CheckConst f
    CheckConst (S1 ('MetaSel ('Just _ ) _ _ _) _) = 'True
    CheckConst (f :*: g) = CheckConst f || CheckConst g
    CheckConst _ = 'False

type family NamedSumError (t :: k) :: ErrorMessage where
    NamedSumError t =
             'Text "'elm-street' doesn't support Sum types with records."
        ':$$: 'Text "But '" ':<>: 'ShowType t ':<>: 'Text "' has records."

-- | Convenience grouping of constraints that type has to satisfy
-- in order to be eligible for automatic derivation of Elm instance via generics
type ElmStreetGenericConstraints a =
    ( HasNoTypeVars a
    , HasLessThanEightUnnamedFields a
    , HasNoNamedSum a
    , Generic a
    , GenericElmDefinition (Rep a)
    )
