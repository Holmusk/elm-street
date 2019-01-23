{- | AST representing structure of Elm types. Haskell generic representation is
converted to this AST which later is going to be pretty-printed.
-}

module Elm.Ast
       ( Definition (..)
       , ElmAlias (..)
       , RecordField (..)
       , TypeName (..)
       , ElmType (..)
       , Constructor (..)
       ) where

import Data.List.NonEmpty (NonEmpty)
import Data.Text (Text)


data Definition
    = DefAlias ElmAlias
    | DefType  ElmType

data ElmAlias = ElmAlias
    { elmAliasName   :: Text  -- ^ Name of the alias
    , elmAliasFields :: NonEmpty RecordField  -- ^ List of fields
    }

data RecordField = RecordField
    { recordFieldName :: Text
    , recordFieldType :: TypeName
    }

newtype TypeName = TypeName
    { unTypeName :: Text
    }

data ElmType = ElmType
    { elmTypeName         :: Text  -- ^ Name of the data type
    , elmTypeVars         :: [Text]  -- ^ List of type variables; currently only phantom variables
    , elmTypeConstructors :: NonEmpty Constructor  -- ^ List of constructors
    }

data Constructor = Constructor
    { constructorName   :: Text  -- ^ Name of the constructor
    , constructorFields :: [TypeName]  -- ^ Fields of the constructor
    }
