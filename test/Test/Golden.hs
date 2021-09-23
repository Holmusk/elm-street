module Test.Golden
       ( goldenSpec
       ) where

import Test.Hspec (Spec, describe, it, runIO, shouldBe)

import Types (OneType, defaultOneType)

import Data.Aeson as A
import Data.ByteString.Lazy as LBS


goldenSpec :: Spec
goldenSpec = describe "golden tests" $ do
    golden <- runIO $ LBS.readFile "test/golden/oneType.json"

    it "Golden JSON -> Haskell == default" $
        A.eitherDecode @OneType golden `shouldBe` Right defaultOneType
    it "default -> JSON -> Haskell == default" $
        (A.eitherDecode @OneType $ A.encode defaultOneType) `shouldBe` Right defaultOneType
