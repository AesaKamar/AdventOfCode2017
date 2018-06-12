module Main where

import           Lib
import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main =
  hspec $ do
    describe "spin" $ do
      it "s0 abcde -> abcde" $ (spin "abcde" 0) `shouldBe` "abcde"
      it "s3 abcde -> cdeab" $ (spin "abcde" 1) `shouldBe` "eabcd"
