module Main where
import           Test.Tasty
import           Test.Tasty.Hspec

main :: IO ()
main = hspec $
  describe "something" $ do
    it "blah" $
      1 `shouldBe` 1
