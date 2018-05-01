module Main where

import           Lib
import           Test.Tasty.Hspec

main :: IO ()
main = hspec $ do
  describe "Flipping a string" $ do
    it "should tie with a single number" $
      (tieKnot [0..4] 0 3) `shouldBe` [2, 1, 0, 3, 4]
