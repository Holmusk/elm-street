{-# LANGUAGE TypeApplications #-}

{- | Generates Elm types from Haskell @types@ internal library.

The generated files can be found in the @elm-example/src@ folder.
-}

module Main (main) where

import Data.Text (Text)
import Elm (defaultSettings, generateElm)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import Types (Types)

import qualified Data.Text as T
import qualified Data.Text.IO as TIO


main :: IO ()
main = do
    -- Generate Types, Encoders, Decoders
    generateElm @Types $ defaultSettings "frontend/src" ["Core"]

    -- Generate JSON string in Elm files for testing purposes
    golden <- TIO.readFile "test/golden/oneType.json"
    generateGoldenType golden

{- |
1. Reads the JSON file created for testing: @test/golden/oneType.json@
2. Creates Elm module with the function that is the string of the read content.
   This file can be found at @frontend/tests/Tests/Golden.elm@.
-}
generateGoldenType :: Text -> IO ()
generateGoldenType c = do
    let path = "frontend/tests/Tests"
    createDirectoryIfMissing True path
    TIO.writeFile (path </> "Golden.elm") golden
  where
    golden :: Text
    golden = T.unlines $
        [ "module Tests.Golden exposing (goldenOneTypeJson)"
        , ""
        , "goldenOneTypeJson : String"
        , "goldenOneTypeJson ="
        ] ++ map ("    " <>)
        ( "\"\"\"" : T.lines c ++ [ "\"\"\"" ]
        )
