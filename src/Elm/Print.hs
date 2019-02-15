{- | This module defines prettyprinter for 'ElmDefinition' type.
and exports the function to represent it in the convenient way.
-}

module Elm.Print
       ( prettyShowDefinition
       ) where

import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, colon, comma, dquotes, emptyDoc, equals, lbrace, line,
                                  lparen, nest, pipe, pretty, prettyList, rbrace, rparen, sep,
                                  space, vsep, (<+>))

import Elm.Ast (ElmAlias (..), ElmConstructor (..), ElmDefinition (..), ElmPrim (..),
                ElmRecordField (..), ElmType (..), TypeName (..), TypeRef (..), getConstructorNames,
                isEnum)

import qualified Data.Text as T


{- | Pretty shows Elm types.

TODO: more docs later
-}
prettyShowDefinition :: ElmDefinition -> Text
prettyShowDefinition = showDoc . elmDoc

showDoc :: Doc ann -> Text
showDoc = T.pack . show

elmDoc :: ElmDefinition -> Doc ann
elmDoc = \case
    DefAlias elmAlias -> elmAliasDoc elmAlias
    DefType elmType -> elmTypeDoc elmType
    DefPrim _ -> emptyDoc

-- | Pretty printer for type reference.
elmTypeRefDoc :: TypeRef -> Doc ann
elmTypeRefDoc = \case
    RefPrim elmPrim -> elmPrimDoc elmPrim
    RefCustom (TypeName typeName) -> pretty typeName

{- | Pretty printer for primitive Elm types. This pretty printer is used only to
display types of fields.
-}
elmPrimDoc :: ElmPrim -> Doc ann
elmPrimDoc = \case
    ElmUnit -> "()"
    ElmNever -> "Never"
    ElmBool -> "Bool"
    ElmChar -> "Char"
    ElmInt -> "Int"
    ElmFloat -> "Float"
    ElmString -> "String"
    ElmMaybe ref -> "Maybe" <+> elmTypeParenDoc ref
    ElmResult refA refB -> "Result" <+> elmTypeParenDoc refA <+> elmTypeParenDoc refB
    ElmPair refA refB -> lparen <> elmTypeRefDoc refA <> comma <+> elmTypeRefDoc refB <> rparen
    ElmList ref -> "List" <+> elmTypeParenDoc ref

{- | Pretty-printer for types. Adds parens for both sides when needed (when type
contains of multiple words).
-}
elmTypeParenDoc :: TypeRef -> Doc ann
elmTypeParenDoc = wordsDoc . T.words . showDoc . elmTypeRefDoc
  where
    wordsDoc :: [Text] -> Doc ann
    wordsDoc = \case
        [] -> ""
        [x] -> pretty x
        xs -> lparen <> pretty (T.unwords xs) <> rparen

{- | Pretty printer for Elm aliases:

@
type alias User =
    { userHeh : String
    , userMeh : Int
    }
@
-}
elmAliasDoc :: ElmAlias -> Doc ann
elmAliasDoc ElmAlias{..} = nest 4 $
    vsep $ ("type alias" <+> pretty elmAliasName <+> equals)
         : fieldsDoc elmAliasFields
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

If the type is Enum this function will add enum specific functions:

type Status
    = Approved
    | Yoyoyo
    | Wow

showStatus : Status -> String
showStatus x = case x of
    Approved -> "Approved"
    Yoyoyo -> "Yoyoyo"
    Wow -> "Wow"

readStatus : String -> Maybe Status
readStatus x = case x of
    "Approved" -> Just Approved
    "Yoyoyo" -> Just Yoyoyo
    "Wow" -> Just Wow
    _ -> Nothing

universeStatus : List Status
universeStatus = [Approved, Yoyoyo, Wow]
-}
elmTypeDoc :: ElmType -> Doc ann
elmTypeDoc t@ElmType{..} =
    nest 4 ( vsep $ ("type" <+> pretty elmTypeName <> sepVars)
                  : constructorsDoc elmTypeConstructors
           )
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
        pretty elmConstructorName : map elmTypeRefDoc elmConstructorFields

    enumFuncs :: Doc ann
    enumFuncs =
        if isEnum t
        then vsep $ map (line <>) [elmEnumShowDoc t, elmEnumReadDoc t, elmEnumUniverse t]
        else emptyDoc

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
    , universeName <+> equals <+> prettyList (getConstructorNames t)
    ]
  where
    universeName :: Doc ann
    universeName = "universe" <> pretty elmTypeName

arrow :: Doc ann
arrow = "->"

{-
putStrLn $ T.unpack $ prettyShowDefinition $ DefAlias $ ElmAlias "User" $ (ElmRecordField (TypeName "String") "userHeh") :| [ElmRecordField (TypeName "Int") "userMeh"]

ENUM:
putStrLn $ T.unpack $ prettyShowDefinition $ DefType $ ElmType "Status" [] $ (ElmConstructor "Approved" []) :| [ElmConstructor  "Yoyoyo" [], ElmConstructor "Wow" []]

putStrLn $ T.unpack $ prettyShowDefinition $ DefType $ ElmType "Status" [] $ (ElmConstructor "Approved" [TypeName "String", TypeName "Int"]) :| [ElmConstructor  "Yoyoyo" [], ElmConstructor "Wow" [TypeName "a"]]

putStrLn $ T.unpack $ prettyShowDefinition $ DefType $ ElmType "Status" ["a"] $ (ElmConstructor "Approved" [TypeName "String", TypeName "Int"]) :| [ElmConstructor  "Yoyoyo" [], ElmConstructor "Wow" [TypeName "a"]]
-}
