{- | This module contains some commonly used function for working
with 'Doc's and pretty printing.
-}

module Elm.Print.Common
       ( showDoc
       , wrapParens
       , arrow
       , mkQualified
       , typeWithVarsDoc
       , qualifiedTypeWithVarsDoc
       ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, concatWith, parens, pretty, surround, (<+>))

import qualified Data.Text as T


-- | Shows pretty-printed document.
showDoc :: Doc ann -> Text
showDoc = T.pack . show

{- | Wraps given document in parens if it contains more than single word.
-}
wrapParens :: Doc ann -> Doc ann
wrapParens doc = case T.words $ showDoc doc of
    []  -> doc
    [_] -> doc
    _   -> parens doc

-- | Pretty printed arrow (@->@).
arrow :: Doc ann
arrow = "->"

{- | Add qualified prefix to the type names or functions:

@
T.MyType

T.showMyType
@

Here we add @T.@ prefix as we only use qualified imports
for @Types as T@ module.
-}
mkQualified :: Text -> Doc ann
mkQualified = pretty . ("T." <>)

{- | Creates a 'Doc' of the type with its type variables (if any).
-}
typeWithVarsDoc
    :: Bool  -- ^ Is qualified
    -> Text  -- ^ Type name
    -> [Text] -- ^ List of type variables
    -> Doc ann
typeWithVarsDoc isQualified typeName = \case
    []   -> tName
    vars -> tName <+> typeVarsDoc vars
  where
    typeVarsDoc :: [Text] -> Doc ann
    typeVarsDoc = concatWith (surround " ") . map pretty
    tName :: Doc ann
    tName =
        if isQualified
        then mkQualified typeName
        else pretty typeName

{- | Creates a 'Doc' of the qualified type with its type variables (if any).
-}
qualifiedTypeWithVarsDoc
    :: Text  -- ^ Type name
    -> [Text] -- ^ List of type variables
    -> Doc ann
qualifiedTypeWithVarsDoc = typeWithVarsDoc True
