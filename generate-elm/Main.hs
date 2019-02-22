{-# LANGUAGE TypeApplications #-}

{- | Generates Elm types from Haskell @types@ internal library.

The generated files can be found in the @elm-example/src@ folder.
-}

module Main (main) where

import Elm (defaultSettings, generateElm)
import Types (Types)


main :: IO ()
main = generateElm @Types $ defaultSettings "frontend/src" ["Core"]
