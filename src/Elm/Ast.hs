{- | AST representing structure of Elm types. Haskell generic representation is
converted to this AST which later is going to be pretty-printed.
-}

module Elm.Ast
       ( ElmDefinition (..)
       , ElmAlias (..)
       , ElmRecordField (..)
       , TypeName (..)
       , ElmType (..)
       , ElmConstructor (..)

       , isEnum
       , getConstructorNames
       ) where

import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)


data ElmDefinition
    = DefAlias ElmAlias
    | DefType  ElmType
    deriving (Show)

data ElmAlias = ElmAlias
    { elmAliasName   :: Text  -- ^ Name of the alias
    , elmAliasFields :: NonEmpty ElmRecordField  -- ^ List of fields
    } deriving (Show)

data ElmRecordField = ElmRecordField
    { elmRecordFieldType :: TypeName
    , elmRecordFieldName :: Text
    } deriving (Show)

newtype TypeName = TypeName
    { unTypeName :: Text
    } deriving (Show)

data ElmType = ElmType
    { elmTypeName         :: Text  -- ^ Name of the data type
    , elmTypeVars         :: [Text]  -- ^ List of type variables; currently only phantom variables
    , elmTypeConstructors :: NonEmpty ElmConstructor  -- ^ List of constructors
    } deriving (Show)

data ElmConstructor = ElmConstructor
    { elmConstructorName   :: Text  -- ^ Name of the constructor
    , elmConstructorFields :: [TypeName]  -- ^ Fields of the constructor
    } deriving (Show)

-- | Checks i the given 'ElmType' is Enum.
isEnum :: ElmType -> Bool
isEnum ElmType{..} = null elmTypeVars && null (foldMap elmConstructorFields elmTypeConstructors)

-- | Gets the list of the constructor names.
getConstructorNames :: ElmType -> [Text]
getConstructorNames ElmType{..} = map elmConstructorName $ toList elmTypeConstructors
