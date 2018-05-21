module Main where

import           Lib
import           Test.Tasty
import           Test.Tasty.Hspec


main :: IO ()
main = hspec $ do
  describe "a test" $ do
    it "in length 3, picosecond 3" $
      moveSecurityBot 3 0 `shouldBe` 0
    it "in length 3, picosecond 2" $
      moveSecurityBot 3 2 `shouldBe` 2
    it "in length 3, picosecond 3" $
      moveSecurityBot 3 3 `shouldBe` 1
    it "in length 5, picosecond 11" $
      moveSecurityBot 5 11 `shouldBe` 3
