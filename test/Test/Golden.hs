module Test.Golden (goldenSpec) where

import Test.Hspec (Spec, describe, it, shouldBe, shouldReturn)

import Types (CustomCodeGen, OneType, defaultCustomCodeGen, defaultOneType)

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as LBS

goldenSpec :: Spec
goldenSpec = describe "golden tests" $ do
    describe "Default CodeGenSettings" $ do
        it "Golden JSON -> Haskell == default" $
            A.eitherDecode @OneType <$> LBS.readFile "test/golden/oneType.json"
                `shouldReturn` Right defaultOneType
        it "default -> JSON -> Haskell == default" $
            A.eitherDecode @OneType (A.encode defaultOneType)
                `shouldBe` Right defaultOneType
    describe "Custom CodeGenSettings" $ do
        it "should decode type with custom CodeGenSettings" $
            A.eitherDecode @CustomCodeGen "{\"customFunTestInt\": 78,\"customFunTestString\": \"Hello\",\"tag\": \"CustomCodeGen\"}"
                `shouldBe` Right defaultCustomCodeGen
        it "should encode type with custom CodeGen" $
            A.eitherDecode @CustomCodeGen (A.encode defaultCustomCodeGen)
                `shouldBe` Right defaultCustomCodeGen
