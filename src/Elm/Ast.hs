{- | AST representing structure of Elm types. Haskell generic representation is
converted to this AST which later is going to be pretty-printed.
-}

module Elm.Ast
       ( ElmDefinition (..)

       , ElmPrim (..)
       , ElmRecord (..)
       , ElmType (..)
       , ElmBuiltin (..)

       , ElmRecordField (..)
       , ElmConstructor (..)
       , isEnum
       , getConstructorNames

       , TypeName (..)
       , TypeRef (..)
       , definitionToRef
       ) where

import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)


-- | Elm data type definition.
data ElmDefinition
    = DefPrim   !ElmPrim
    | DefRecord !ElmRecord
    | DefBuiltin !ElmBuiltin
    | DefType   !ElmType
    deriving (Show)

-- | AST for @record type alias@ in Elm.
data ElmRecord = ElmRecord
    { elmRecordName      :: !Text  -- ^ Name of the record
    , elmRecordFields    :: !(NonEmpty ElmRecordField)  -- ^ List of fields
    , elmRecordIsNewtype :: !Bool  -- ^ 'True' if Haskell type is a @newtype@
    } deriving (Show)

-- | Single field of @record type alias@.
data ElmRecordField = ElmRecordField
    { elmRecordFieldType :: !TypeRef
    , elmRecordFieldName :: !Text
    } deriving (Show)

-- | Wrapper for name of the type.
newtype TypeName = TypeName
    { unTypeName :: Text
    } deriving (Show)

-- | AST for @type@ in Elm.
data ElmType = ElmType
    { elmTypeName         :: !Text  -- ^ Name of the data type
    , elmTypeVars         :: ![Text]  -- ^ List of type variables; currently only phantom variables
    , elmTypeIsNewtype    :: !Bool  -- ^ 'True' if Haskell type is a @newtype@
    , elmTypeConstructors :: !(NonEmpty ElmConstructor)  -- ^ List of constructors
    } deriving (Show)

-- | Constructor of @type@.
data ElmConstructor = ElmConstructor
    { elmConstructorName   :: !Text  -- ^ Name of the constructor
    , elmConstructorFields :: ![TypeRef]  -- ^ Fields of the constructor
    } deriving (Show)

-- | Checks if the given 'ElmType' is Enum.
isEnum :: ElmType -> Bool
isEnum ElmType{..} = null elmTypeVars && null (foldMap elmConstructorFields elmTypeConstructors)

-- | Gets the list of the constructor names.
getConstructorNames :: ElmType -> [Text]
getConstructorNames ElmType{..} = map elmConstructorName $ toList elmTypeConstructors

-- | Primitive elm types which are parts of a language
data ElmPrim
    = ElmUnit                               -- ^ @()@ type in elm
    | ElmNever                              -- ^ @Never@ type in elm, analogous to Void in Haskell
    | ElmBool                               -- ^ @Bool@
    | ElmChar                               -- ^ @Char@
    | ElmInt                                -- ^ @Int@
    | ElmFloat                              -- ^ @Float@
    | ElmString                             -- ^ @String@
    | ElmPair !TypeRef !TypeRef             -- ^ @(A, B)@ in elm
    | ElmTriple !TypeRef !TypeRef !TypeRef  -- ^ @(A, B, C)@ in elm
    deriving (Show)

-- | Buitin types defined by core or 3rd party libraries
data ElmBuiltin
    = ElmMaybe !TypeRef                     -- ^ @Maybe T@ part of @elm/core@
    | ElmResult !TypeRef !TypeRef           -- ^ @Result A B@ part of @elm/core@
    | ElmList !TypeRef                      -- ^ @List A@ part of @elm/core@
    | ElmTime                               -- ^ @Posix@ in elm, @UTCTime@ in Haskell use @elm/time@
    | ElmValue                              -- ^ @Json.Encode.Value@ in elm, @Data.Aeson.Value@ in Haskell use @elm/json@
    | ElmNonEmptyPair !TypeRef              -- ^ @NonEmpty A@ represented by @(A, List A)@ in elm see @turboMaCk/non-empty-list-alias@
    deriving (Show)

-- | Reference to another existing type.
data TypeRef
    = RefPrim !ElmPrim
    | RefCustom !TypeName
    | RefBuiltin !ElmBuiltin
    deriving (Show)

-- | Extracts reference to the existing data type type from some other type elm defintion.
definitionToRef :: ElmDefinition -> TypeRef
definitionToRef = \case
    DefRecord ElmRecord{..} -> RefCustom $ TypeName elmRecordName
    DefType ElmType{..}     -> RefCustom $ TypeName elmTypeName
    DefPrim elmPrim         -> RefPrim elmPrim
    DefBuiltin elmBuiltIn   -> RefBuiltin elmBuiltIn
