{- | This module defines prettyprinter for 'ElmDefinition' type.
and exports the function to represent it in the convenient way.
-}

module Elm.Print
       ( prettyShowDefinition
       , prettyShowEncoder

         -- * Standard missing encoders
       , encodeMaybe
       , encodeEither
       , encodePair
       ) where

import Data.List.NonEmpty (NonEmpty ((:|)), toList)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, brackets, colon, comma, concatWith, dquotes, emptyDoc,
                                  equals, lbrace, lbracket, line, lparen, nest, parens, pipe,
                                  pretty, prettyList, rbrace, rbracket, rparen, sep, space,
                                  surround, vsep, (<+>))

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

----------------------------------------------------------------------------
-- Encode
----------------------------------------------------------------------------

{- | Returns the encoder for the given type.


TODO
 +-------------------+------------------+-----------------+---------------------+
 |    Haskell Type   |     Eml Type     |     Encoder     |       JSON          |
 +===================+==================+=================+=====================+
 |   'Int'           |      'Int'       | standard encoder |                    |
 +-------------------+------------------+------------------+--------------------+

-}
prettyShowEncoder :: ElmDefinition -> Text
prettyShowEncoder def = showDoc $ case def of
    DefAlias elmAlias -> aliasEncoderDoc elmAlias
    DefType elmType   -> typeEncoderDoc elmType
    DefPrim _         -> emptyDoc

-- | Encoder for 'ElmType' (which is either enum or the Sum type).
typeEncoderDoc :: ElmType -> Doc ann
typeEncoderDoc t@ElmType{..} =
    -- function defenition: @encodeTypeName : TypeName -> Value@.
       encoderDef elmTypeName elmTypeVars
    <> line
    <> if isEnum t
       -- if this is Enum just using the show instance we wrote.
       then name <+> equals <+> "E.string << show" <> pretty elmTypeName
       -- If it sum type then it should look like: @{"tag": "Foo", "contents" : ["string", 1]}@
       else nest 4 $ vsep $ (name <+> "x" <+> equals <+> "E.object <| case x of")
            : map mkCase (toList elmTypeConstructors)
  where
    -- | Encoder function name
    name :: Doc ann
    name = encoderName elmTypeName

    -- | Create case clouse for each of the sum Constructors.
    mkCase :: ElmConstructor -> Doc ann
    mkCase ElmConstructor{..} =
        conName <+> vars <+> arrow
            <+> brackets (parens ("tag" <> comma <+> dquotes conName) <> contents)
      where
        -- | Constructor name
        conName :: Doc ann
        conName = pretty elmConstructorName

        -- | Creates variables: @x1@ to @xN@, where N is the number of the constructor fields.
        fields :: [Doc ann]
        fields = take (length elmConstructorFields) $
            map (pretty . mkText "x") [1..]

        contents :: Doc ann
        contents = "," <+> parens (dquotes "contents" <> comma <+> "E.list" <+> brackets fieldEncs)

        -- | @encoderA x1@
        fieldEncs :: Doc ann
        fieldEncs = concatWith (surround ", ") $
            zipWith (<+>) (map typeRefEncoder elmConstructorFields) fields

        -- | Makes variable like: @x11@ etc.
        mkText :: Text -> Int -> Text
        mkText x i = x <> T.pack (show i)

        vars :: Doc ann
        vars =  concatWith (surround " ") fields


aliasEncoderDoc :: ElmAlias -> Doc ann
aliasEncoderDoc ElmAlias{..} =
    encoderDef elmAliasName []
    <> line
    <> nest 4 (vsep $ (encoderName elmAliasName <+> "x" <+> equals <+> "E.object")
            : fieldsEncode elmAliasFields)
  where
    fieldsEncode :: NonEmpty ElmRecordField -> [Doc ann]
    fieldsEncode (fstR :| rest) =
        lbracket <+> recordFieldDoc fstR
      : map ((comma <+>) . recordFieldDoc) rest
     ++ [rbracket]

    recordFieldDoc :: ElmRecordField -> Doc ann
    recordFieldDoc ElmRecordField{..} = parens $
            dquotes (pretty elmRecordFieldName)
         <> comma
        <+> typeRefEncoder elmRecordFieldType
        <+> "x." <> pretty elmRecordFieldName

-- | The definition of the @encodeTYPENAME@ function.
encoderDef
    :: Text  -- ^ Type name
    -> [Text] -- ^ List of type variables
    -> Doc ann
encoderDef typeName vars =
    encoderName typeName <+> colon <+> pretty typeName <> typeVarsDoc <+> arrow <+> "Value"
  where
    typeVarsDoc :: Doc ann
    typeVarsDoc = concatWith (surround " ") $ map pretty vars

-- | Create the name of the encoder function.
encoderName :: Text -> Doc ann
encoderName typeName = "encode" <> pretty typeName

-- | Converts the reference to the existing type to the corresponding encoder.
typeRefEncoder :: TypeRef -> Doc ann
typeRefEncoder (RefPrim elmPrim) = case elmPrim of
    ElmUnit       -> "const E.null"
    ElmNever      -> "never"
    ElmBool       -> "E.bool"
    ElmChar       -> parens "E.string << String.fromChar"
    ElmInt        -> "E.int"
    ElmFloat      -> "E.float"
    ElmString     -> "E.string"
    ElmMaybe t    -> parens $ "encodeMaybe" <+> typeRefEncoder t
    ElmResult l r -> parens $ "encodeEither" <+> typeRefEncoder l <+> typeRefEncoder r
    ElmPair a b   -> parens $ "encodePair" <+> typeRefEncoder a <+> typeRefEncoder b
    ElmList l     -> "E.list" <+> typeRefEncoder l
typeRefEncoder (RefCustom TypeName{..}) = "encode" <> pretty unTypeName

encodeMaybe :: Doc ann
encodeMaybe = vsep
    [ "encodeMaybe : (a -> Value) -> Maybe a -> Value"
    , "encodeMaybe enc = Maybe.withDefault E.null << Maybe.map enc"
    ]

encodeEither :: Doc ann
encodeEither = vsep
    [ "encodeEither : (a -> Value) -> (b -> Value) -> Result a b -> Value"
    , "encodeEither encA encB res = E.object <| case res of"
    , "    Err a -> [(\"Left\",  encA a)]"
    , "    Ok b  -> [(\"Right\", encB b)]"
    ]

encodePair :: Doc ann
encodePair = vsep
    [ "encodePair : (a -> Value) -> (b -> Value) -> (a, b) -> Value"
    , "encodePair encA encB (a, b) = E.list [encA a, encB b]"
    ]

{-
putStrLn $ T.unpack $ prettyShowDefinition $ DefAlias $ ElmAlias "User" $ (ElmRecordField (TypeName "String") "userHeh") :| [ElmRecordField (TypeName "Int") "userMeh"]

ENUM:
putStrLn $ T.unpack $ prettyShowDefinition $ DefType $ ElmType "Status" [] $ (ElmConstructor "Approved" []) :| [ElmConstructor  "Yoyoyo" [], ElmConstructor "Wow" []]
putStrLn $ T.unpack $ prettyShowEncoder $ DefType $ ElmType "Status" [] $ (ElmConstructor "Approved" []) :| [ElmConstructor  "Yoyoyo" [], ElmConstructor "Wow" []]

putStrLn $ T.unpack $ prettyShowDefinition $ DefType $ ElmType "Status" [] $ (ElmConstructor "Approved" [TypeName "String", TypeName "Int"]) :| [ElmConstructor  "Yoyoyo" [], ElmConstructor "Wow" [TypeName "a"]]

putStrLn $ T.unpack $ prettyShowDefinition $ DefType $ ElmType "Status" ["a"] $ (ElmConstructor "Approved" [TypeName "String", TypeName "Int"]) :| [ElmConstructor  "Yoyoyo" [], ElmConstructor "Wow" [TypeName "a"]]
-}
