{- | This module defines prettyprinter for 'ElmDefinition' type.
and exports the function to represent it in the convenient way.
-}

module Elm.Print
       ( module Elm.Print.Common
       , module Elm.Print.Decoder
       , module Elm.Print.Encoder
       , module Elm.Print.Types
       ) where

import Elm.Print.Common
import Elm.Print.Decoder
import Elm.Print.Encoder
import Elm.Print.Types

{-
import qualified Data.Text as T
import Elm.Ast
import Data.List.NonEmpty

test :: IO ()
test = do
    putStrLn $ T.unpack $ prettyShowDefinition $ DefAlias $ ElmAlias "User"  (ElmRecordField (RefPrim ElmString) "userHeh" :| [ElmRecordField (RefPrim ElmInt) "userMeh"]) False

    --ENUM:
    putStrLn $ T.unpack $ prettyShowDefinition $ DefType $ ElmType "Status" [] False $ ElmConstructor "Approved" [] :| [ElmConstructor  "Yoyoyo" [], ElmConstructor "Wow" []]
    putStrLn $ T.unpack $ prettyShowEncoder    $ DefType $ ElmType "Status" [] False $ ElmConstructor "Approved" [] :| [ElmConstructor  "Yoyoyo" [], ElmConstructor "Wow" []]
    putStrLn $ T.unpack $ prettyShowDefinition $ DefType $ ElmType "Status" [] False $ ElmConstructor "Approved" [RefPrim ElmString, RefPrim ElmInt] :| [ElmConstructor  "Yoyoyo" [], ElmConstructor "Wow" [RefCustom $ TypeName "a"]]
    putStrLn $ T.unpack $ prettyShowDefinition $ DefType $ ElmType "Status" ["a"] False $ ElmConstructor "Approved" [RefPrim ElmString, RefPrim ElmInt] :| [ElmConstructor  "Yoyoyo" [], ElmConstructor "Wow" [RefCustom $ TypeName "a"]]
    putStrLn $ T.unpack $ prettyShowDefinition $ DefType $ ElmType "Status" [] False (ElmConstructor "Approved" [] :| [ElmConstructor  "Yoyoyo" [], ElmConstructor "Wow" [], ElmConstructor "OneMore" [], ElmConstructor "AndAnother" []])

-}
