{- | This module contains some commonly used function for working
with 'Doc's and pretty printing.
-}

module Elm.Print.Common
       ( showDoc
       , wrapParens
       , arrow
       , mkQualified
       , typeWithVarsDoc
       ) where

import Data.Text (Text)
import Data.Text.Prettyprint.Doc (Doc, concatWith, lparen, pretty, rparen, surround, (<+>))

import qualified Data.Text as T


-- | Shows pretty-printed document.
showDoc :: Doc ann -> Text
showDoc = T.pack . show

{- | Wraps given document in parens if it contains more than single word.
-}
wrapParens :: Doc ann -> Doc ann
wrapParens = wordsDoc . T.words . showDoc
  where
    wordsDoc :: [Text] -> Doc ann
    wordsDoc = \case
        []  -> ""
        [x] -> pretty x
        xs  -> lparen <> pretty (T.unwords xs) <> rparen

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

{- | Creates a 'Doc' of the qualified type with its type variables (if any).
-}
typeWithVarsDoc
    :: Text  -- ^ Type name
    -> [Text] -- ^ List of type variables
    -> Doc ann
typeWithVarsDoc (mkQualified -> qTypeName) = \case
    []   -> qTypeName
    vars -> qTypeName <+> typeVarsDoc vars
  where
    typeVarsDoc :: [Text] -> Doc ann
    typeVarsDoc = concatWith (surround " ") . map pretty
