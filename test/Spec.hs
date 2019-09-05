module Main (main) where

import Test.Hspec (hspec)

import Test.Golden (goldenSpec)


main :: IO ()
main = hspec goldenSpec
