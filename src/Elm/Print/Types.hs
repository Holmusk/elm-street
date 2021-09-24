{- | Pretty functions for `Types.elm` module.

The generated module should contain:

  * Type definitions for all ADT
  * @show*@ functions for Enum types
  * @read*@ functions for Enum types
  * @universe*@ functions for Enum types
  * @un*@ functions for newtypes

==== __Example__

The example of Record, Newtype and Enum generated type and functions:

@
type alias User =
    { id : Id
    , name : String
    , age : Age
    , status : RequestStatus
    }

type RequestStatus
    = Approved
    | Rejected
    | Reviewing

showRequestStatus : RequestStatus -> String
showRequestStatus x = case x of
    Approved -> \"Approved\"
    Rejected -> \"Rejected\"
    Reviewing -> \"Reviewing\"

readRequestStatus : String -> Maybe RequestStatus
readRequestStatus x = case x of
    \"Approved\" -> Just Approved
    \"Rejected\" -> Just Rejected
    \"Reviewing\" -> Just Reviewing
    _ -> Nothing

universeRequestStatus : List RequestStatus
universeRequestStatus = [Approved, Rejected, Reviewing]

type Id
    = Id String

unId : Id -> String
unId (Id x) = x
@

-}

module Elm.Print.Types
       ( prettyShowDefinition

         -- * Internal functions
       , elmRecordDoc
       , elmTypeDoc
       ) where

import Data.List (intersperse)
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, align, colon, comma, dquotes, emptyDoc, equals, lbrace, line,
                                  lparen, nest, parens, pipe, pretty, prettyList, rbrace, rparen,
                                  sep, space, vsep, (<+>))
import Prettyprinter.Util (reflow)

import Elm.Ast (ElmBuiltin (..), ElmConstructor (..), ElmDefinition (..),
                ElmPrim (..), ElmRecord (..), ElmRecordField (..), ElmType (..), TypeName (..),
                TypeRef (..), getConstructorNames, isEnum)
import Elm.Print.Common (arrow, showDoc, typeWithVarsDoc, wrapParens)

import qualified Data.List.NonEmpty as NE


{- | Pretty shows Elm types.

* See 'elmRecordDoc' for examples of generated @record type alias@.
* See 'elmTypeDoc' for examples of generated @type@.
-}
prettyShowDefinition :: ElmDefinition -> Text
prettyShowDefinition = showDoc . elmDoc

elmDoc :: ElmDefinition -> Doc ann
elmDoc = \case
    DefRecord elmRecord -> elmRecordDoc elmRecord
    DefType elmType     -> elmTypeDoc elmType
    DefPrim _           -> emptyDoc
    DefBuiltin _        -> emptyDoc

-- | Pretty printer for type reference.
elmTypeRefDoc :: TypeRef -> Doc ann
elmTypeRefDoc = \case
    RefPrim elmPrim               -> elmPrimDoc elmPrim
    RefCustom (TypeName typeName) -> pretty typeName
    RefBuiltin elmBuiltIn         -> elmBuiltinDoc elmBuiltIn

{- | Pretty printer for primitive Elm types. This pretty printer is used only to
display types of fields.
-}
elmPrimDoc :: ElmPrim -> Doc ann
elmPrimDoc = \case
    ElmUnit           -> "()"
    ElmNever          -> "Never"
    ElmBool           -> "Bool"
    ElmChar           -> "Char"
    ElmInt            -> "Int"
    ElmFloat          -> "Float"
    ElmString         -> "String"
    ElmPair a b       -> lparen <> elmTypeRefDoc a <> comma <+> elmTypeRefDoc b <> rparen
    ElmTriple a b c   -> lparen <> elmTypeRefDoc a <> comma <+> elmTypeRefDoc b <> comma <+> elmTypeRefDoc c <> rparen

elmBuiltinDoc :: ElmBuiltin -> Doc ann
elmBuiltinDoc ElmBuiltin{..} =
    reflow builtinImplType <+> mconcat (intersperse " " (fmap elmTypeParenDoc builtinImplParams))

{- | Pretty-printer for types. Adds parens for both sides when needed (when type
consists of multiple words).
-}
elmTypeParenDoc :: TypeRef -> Doc ann
elmTypeParenDoc = wrapParens . elmTypeRefDoc

{- | Pretty printer for Elm records:

@
type alias User =
    { userHeh : String
    , userMeh : Int
    }
@
-}
elmRecordDoc :: ElmRecord -> Doc ann
elmRecordDoc ElmRecord{..} = nest 4 $
    vsep $ ("type alias" <+> pretty elmRecordName <+> equals)
         : fieldsDoc elmRecordFields
  where
    fieldsDoc :: NonEmpty ElmRecordField -> [Doc ann]
    fieldsDoc (fstR :| rest) =
        lbrace <+> recordFieldDoc fstR
      : map ((comma <+>) . recordFieldDoc) rest
     ++ [rbrace]

    recordFieldDoc :: ElmRecordField -> Doc ann
    recordFieldDoc ElmRecordField{..} =
            pretty elmRecordFieldName
        <+> colon
        <+> elmTypeRefDoc elmRecordFieldType

{- | Pretty printer for Elm types with one or more constructors:

@
type Status a
    = Foo String Int
    | Bar a
    | Baz
@

If the type is a newtype then additionally @unTYPENAME@ function is generated:

@
type Id a
    = Id String

unId : Id a -> String
unId (Id x) = x
@

If the type is Enum this function will add enum specific functions:

@
type Status
    = Approved
    | Yoyoyo
    | Wow

showStatus : Status -> String
showStatus x = case x of
    Approved -> \"Approved\"
    Yoyoyo -> \"Yoyoyo\"
    Wow -> \"Wow\"

readStatus : String -> Maybe Status
readStatus x = case x of
    \"Approved\" -> Just Approved
    \"Yoyoyo\" -> Just Yoyoyo
    \"Wow\" -> Just Wow
    _ -> Nothing

universeStatus : List Status
universeStatus = [Approved, Yoyoyo, Wow]
@
-}
elmTypeDoc :: ElmType -> Doc ann
elmTypeDoc t@ElmType{..} =
    nest 4 ( vsep $ ("type" <+> pretty elmTypeName <> sepVars)
                  : constructorsDoc elmTypeConstructors
           )
    <> unFunc
    <> enumFuncs
  where
    sepVars :: Doc ann
    sepVars = case elmTypeVars of
        []   -> emptyDoc
        vars -> space <> sep (map pretty vars)

    constructorsDoc :: NonEmpty ElmConstructor -> [Doc ann]
    constructorsDoc (fstC :| rest) =
        equals <+> constructorDoc fstC
        : map ((pipe <+>) . constructorDoc) rest

    constructorDoc :: ElmConstructor -> Doc ann
    constructorDoc ElmConstructor{..} = sep $
        pretty elmConstructorName : map elmTypeParenDoc elmConstructorFields

    -- Generates 'unTYPENAME' function for newtype
    unFunc :: Doc ann
    unFunc =
        if elmTypeIsNewtype
        then line <> elmUnFuncDoc t
        else emptyDoc

    enumFuncs :: Doc ann
    enumFuncs =
        if isEnum t
        then vsep $ map (line <>) [elmEnumShowDoc t, elmEnumReadDoc t, elmEnumUniverse t]
        else emptyDoc

elmUnFuncDoc :: ElmType -> Doc ann
elmUnFuncDoc ElmType{..} = line <> vsep
    [ unName <+> colon <+> typeWithVarsDoc False elmTypeName elmTypeVars <+> arrow <+> result
    , unName <+> parens (ctorName <+> "x") <+> equals <+> "x"
    ]
  where
    unName :: Doc ann
    unName = "un" <> pretty elmTypeName

    ctor :: ElmConstructor
    ctor = NE.head elmTypeConstructors

    result :: Doc ann
    result = case elmConstructorFields ctor of
        []      -> "ERROR"
        fld : _ -> elmTypeRefDoc fld

    ctorName :: Doc ann
    ctorName = pretty $ elmConstructorName ctor

elmEnumShowDoc :: forall ann . ElmType -> Doc ann
elmEnumShowDoc t@ElmType{..} =
    line
    -- function type
    <> (showName <+> colon <+> pretty elmTypeName <+> arrow <+> "String")
    <> line
    -- function body
    <> nest 4
        ( vsep $ (showName <+> "x" <+> equals <+> "case x of")
        -- pattern matching
        : map patternMatch (getConstructorNames t)
        )
  where
    showName :: Doc ann
    showName = "show" <> pretty elmTypeName

    patternMatch :: Text -> Doc ann
    patternMatch (pretty -> c) = c <+> arrow <+> dquotes c

elmEnumReadDoc :: ElmType -> Doc ann
elmEnumReadDoc t@ElmType{..} =
    -- function type
    (readName <+> colon <+> "String" <+> arrow <+> "Maybe" <+> pretty elmTypeName)
    <> line
    -- function body
    <> nest 4
        ( vsep $ (readName <+> "x" <+> equals <+> "case x of")
        -- pattern matching
        : map patternMatch (getConstructorNames t)
       ++ ["_" <+> arrow <+> "Nothing"]
        )
  where
    readName :: Doc ann
    readName = "read" <> pretty elmTypeName

    patternMatch :: Text -> Doc ann
    patternMatch (pretty -> c) = dquotes c <+> arrow <+> "Just" <+> c

elmEnumUniverse :: ElmType -> Doc ann
elmEnumUniverse t@ElmType{..} = vsep
    -- function type
    [ universeName <+> colon <+> "List" <+> pretty elmTypeName
    , universeName <+> equals <+> align (prettyList $ getConstructorNames t)
    ]
  where
    universeName :: Doc ann
    universeName = "universe" <> pretty elmTypeName
