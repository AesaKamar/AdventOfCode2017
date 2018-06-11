module Main where

import           Control.Exception (evaluate)
import           Data.Foldable
import           Lib2
import           Test.Tasty.Hspec

main :: IO ()
main =
  hspec $ do
    describe "distance function" $ do
      it "ne,ne,ne is 3 steps away" $
        distance mempty (foldMap move [Ne, Ne, Ne]) `shouldBe` 3
      it "ne,ne,sw,sw is 0 steps away (back where you started)" $
        distance mempty (foldMap move [Ne, Ne, Sw, Sw]) `shouldBe` 0
      it "ne,ne,s,s is 2 steps away (se,se)." $
        distance mempty (foldMap move [Ne, Ne, So, So]) `shouldBe` 2
      it "se,sw,se,sw,sw is 3 steps away (s,s,sw)" $
        distance mempty (foldMap move [Se, Sw, Se, Sw, Sw]) `shouldBe` 3
    -- describe "collapse" $ do
    --   it "0 0 0 should collapse to 0 0 0" $
    --     collapse (HexMove 0 0 0) `shouldBe` (HexMove 0 0 0)
    --   it "2 2 0 should collapse to 0 0 2" $
    --     collapse (HexMove 2 2 0) `shouldBe` (HexMove 0 0 2)
