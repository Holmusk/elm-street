{- | Pretty-printing functions for @Encoder.elm@ module.
Also contains encoders for common types which go to the @ElmStreet.elm@ module.
-}

module Elm.Print.Encoder
       ( prettyShowEncoder

         -- * Standard missing encoders
       , encodeMaybe
       , encodeEither
       , encodePair
       , encodeTriple
       , encodeDict
       ) where

import Data.List.NonEmpty (NonEmpty, toList)
import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, brackets, colon, comma, concatWith, dquotes, emptyDoc,
                                  equals, lbracket, line, nest, parens, pretty, rbracket, surround,
                                  vsep, (<+>))

import Elm.Ast (ElmAlias (..), ElmConstructor (..), ElmDefinition (..), ElmPrim (..),
                ElmRecordField (..), ElmType (..), TypeName (..), TypeRef (..), isEnum)
import Elm.Print.Common (arrow, mkQualified, qualifiedTypeWithVarsDoc, showDoc, wrapParens)

import qualified Data.List.NonEmpty as NE
import qualified Data.Text as T


{- | Returns the encoder for the given type.


TODO

 +-------------------+------------------+------------------+--------------------+
 |    Haskell Type   |     Eml Type     |     Encoder      |       JSON         |
 +===================+==================+==================+====================+
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
       then enumEncoder
       else if elmTypeIsNewtype
            -- if this is type with one constructor and one field then it should just call encoder for wrapped type
            then newtypeEncoder
            -- If it sum type then it should look like: @{"tag": "Foo", "contents" : ["string", 1]}@
            else sumEncoder
  where
    enumEncoder :: Doc ann
    enumEncoder = name <+> equals <+> "E.string << T.show" <> pretty elmTypeName

    newtypeEncoder :: Doc ann
    newtypeEncoder =
        name <+> equals <+> fieldEncoderDoc <+> "<< T.un" <> pretty elmTypeName
      where
        fieldEncoderDoc :: Doc ann
        fieldEncoderDoc = case elmConstructorFields $ NE.head elmTypeConstructors of
            []    -> "ERROR"
            f : _ -> wrapParens (typeRefEncoder f)

    sumEncoder :: Doc ann
    sumEncoder = nest 4
        $ vsep
        $ (name <+> "x" <+> equals <+> "E.object <| case x of")
        : map mkCase (toList elmTypeConstructors)

    -- | Encoder function name
    name :: Doc ann
    name = encoderName elmTypeName

    -- | Create case clouse for each of the sum Constructors.
    mkCase :: ElmConstructor -> Doc ann
    mkCase ElmConstructor{..} = mkQualified elmConstructorName
        <+> vars
        <+> arrow
        <+> brackets (mkTag elmConstructorName <> contents)
      where
        -- | Creates variables: @x1@ to @xN@, where N is the number of the constructor fields.
        fields :: [Doc ann]
        fields = take (length elmConstructorFields) $
            map (pretty . mkText "x") [1..]

        contents :: Doc ann
        contents = "," <+> parens (dquotes "contents" <> comma <+> contentsEnc)

        -- JSON encoder for the "contents" key
        contentsEnc :: Doc ann
        contentsEnc = case elmConstructorFields of
            [_] -> fieldEncs
            _   -> "E.list identity" <+> brackets fieldEncs

        -- | @encoderA x1@
        fieldEncs :: Doc ann
        fieldEncs = concatWith (surround ", ") $
            zipWith (<+>) (map (wrapParens . typeRefEncoder) elmConstructorFields) fields

        -- | Makes variable like: @x11@ etc.
        mkText :: Text -> Int -> Text
        mkText x i = x <> T.pack (show i)

        vars :: Doc ann
        vars =  concatWith (surround " ") fields


aliasEncoderDoc :: ElmAlias -> Doc ann
aliasEncoderDoc ElmAlias{..} =
    encoderDef elmAliasName []
    <> line
    <> if elmAliasIsNewtype
       then newtypeEncoder
       else recordEncoder
  where
    newtypeEncoder :: Doc ann
    newtypeEncoder = leftPart <+> fieldEncoderDoc (NE.head elmAliasFields)

    recordEncoder :: Doc ann
    recordEncoder = nest 4
        $ vsep
        $ (leftPart <+> "E.object")
        : fieldsEncode elmAliasFields

    leftPart :: Doc ann
    leftPart = encoderName elmAliasName <+> "x" <+> equals

    fieldsEncode :: NonEmpty ElmRecordField -> [Doc ann]
    fieldsEncode fields =
        lbracket <+> mkTag elmAliasName
      : map ((comma <+>) . recordFieldDoc) (NE.toList fields)
     ++ [rbracket]

    recordFieldDoc :: ElmRecordField -> Doc ann
    recordFieldDoc field@ElmRecordField{..} = parens $
            dquotes (pretty elmRecordFieldName)
         <> comma
        <+> fieldEncoderDoc field

    fieldEncoderDoc :: ElmRecordField -> Doc ann
    fieldEncoderDoc ElmRecordField{..} =
        wrapParens (typeRefEncoder elmRecordFieldType) <+> "x." <> pretty elmRecordFieldName

-- | Create pair of view: @("tag", E.string "SomeName")@.
mkTag :: Text -> Doc ann
mkTag txt = parens $ dquotes "tag" <> comma <+> "E.string" <+> dquotes (pretty txt)

-- | The definition of the @encodeTYPENAME@ function.
encoderDef
    :: Text  -- ^ Type name
    -> [Text] -- ^ List of type variables
    -> Doc ann
encoderDef typeName vars =
    encoderName typeName
    <+> colon
    <+> qualifiedTypeWithVarsDoc typeName vars
    <+> arrow
    <+> "Value"

-- | Create the name of the encoder function.
encoderName :: Text -> Doc ann
encoderName typeName = "encode" <> pretty typeName

-- | Converts the reference to the existing type to the corresponding encoder.
typeRefEncoder :: TypeRef -> Doc ann
typeRefEncoder (RefCustom TypeName{..}) = "encode" <> pretty (T.takeWhile (/= ' ') unTypeName)
typeRefEncoder (RefPrim elmPrim) = case elmPrim of
    ElmUnit         -> "always <| E.list identity []"
    ElmNever        -> "never"
    ElmBool         -> "E.bool"
    ElmChar         -> "E.string << String.fromChar"
    ElmInt          -> "E.int"
    ElmFloat        -> "E.float"
    ElmString       -> "E.string"
    ElmTime         -> "Iso.encode"
    ElmMaybe t      -> "elmStreetEncodeMaybe"
        <+> wrapParens (typeRefEncoder t)
    ElmResult l r   -> "elmStreetEncodeEither"
        <+> wrapParens (typeRefEncoder l)
        <+> wrapParens (typeRefEncoder r)
    ElmPair a b     -> "elmStreetEncodePair"
        <+> wrapParens (typeRefEncoder a)
        <+> wrapParens (typeRefEncoder b)
    ElmTriple a b c -> "elmStreetEncodeTriple"
        <+> wrapParens (typeRefEncoder a)
        <+> wrapParens (typeRefEncoder b)
        <+> wrapParens (typeRefEncoder c)
    ElmList l       -> "E.list" <+> wrapParens (typeRefEncoder l)
    ElmDict k v     -> "elmStreetEncodeDict"
        <+> wrapParens (typeRefEncoder k)
        <+> wrapParens (typeRefEncoder v)

-- | @JSON@ encoder Elm help function for 'Maybe's.
encodeMaybe :: Text
encodeMaybe = T.unlines
    [ "elmStreetEncodeMaybe : (a -> Value) -> Maybe a -> Value"
    , "elmStreetEncodeMaybe enc = Maybe.withDefault E.null << Maybe.map enc"
    ]

-- | @JSON@ encoder Elm help function for 'Either's.
encodeEither :: Text
encodeEither = T.unlines
    [ "elmStreetEncodeEither : (a -> Value) -> (b -> Value) -> Result a b -> Value"
    , "elmStreetEncodeEither encA encB res = E.object <| case res of"
    , "    Err a -> [(\"Left\",  encA a)]"
    , "    Ok b  -> [(\"Right\", encB b)]"
    ]

-- | @JSON@ encoder Elm help function for 2-tuples.
encodePair :: Text
encodePair = T.unlines
    [ "elmStreetEncodePair : (a -> Value) -> (b -> Value) -> (a, b) -> Value"
    , "elmStreetEncodePair encA encB (a, b) = E.list identity [encA a, encB b]"
    ]

-- | @JSON@ encoder Elm help function for 3-tuples.
encodeTriple :: Text
encodeTriple = T.unlines
    [ "elmStreetEncodeTriple : (a -> Value) -> (b -> Value) -> (c -> Value) -> (a, b, c) -> Value"
    , "elmStreetEncodeTriple encA encB encC (a, b, c) = E.list identity [encA a, encB b, encC c]"
    ]

encodeDict :: Text
encodeDict = T.unlines
    [ "elmStreetEncodeDict : (k -> Value) -> (v -> Value) -> Dict k v -> Value"
    , "elmStreetEncodeDict encK encV = E.dict (E.encode 0 << encK) encV"
    ]
