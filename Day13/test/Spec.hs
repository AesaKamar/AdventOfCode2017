module Main where

import           Lib
import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main =
  hspec $ do
    describe "security bots" $ do
      it "in length 3, picosecond 0" $ moveSecurityBot 3 0 `shouldBe` 0
      it "in length 3, picosecond 2" $ moveSecurityBot 3 2 `shouldBe` 2
      it "in length 3, picosecond 3" $ moveSecurityBot 3 3 `shouldBe` 1
      it "in length 5, picosecond 11" $ moveSecurityBot 5 11 `shouldBe` 3
    describe "moving the scanner" $ do
      it "should move correctly" $ (moveScanner [] 0 columns) `shouldBe` [0, 6]
  where
    columns =
      [ Just (Column 0 3)
      , Just (Column 1 2)
      , Nothing
      , Nothing
      , Just (Column 4 4)
      , Nothing
      , Just (Column 6 4)
      ]
