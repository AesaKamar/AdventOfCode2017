module Main where

import           Lib

import           Control.Exception (evaluate)
import           Data.Map.Strict   as Map
import           Test.Tasty.Hspec
import           Text.Parsec       (parse)

main :: IO ()
main = hspec $ do
  describe "Parsing" $ do
    "b inc 5 if a > 1" `shouldParseAs`
      Right( Statement "b" (Increment 5) ("a", GreaterThan, 1))
    "a inc 1 if b < 5" `shouldParseAs`
      Right( Statement "a" (Increment 1) ("b", LessThan, 5))
    "c dec -10 if a >= 1" `shouldParseAs`
      Right( Statement "c" (Decrement (-10)) ("a", GreaterThanOrEqualTo, 1))
    "c inc -20 if c == 10" `shouldParseAs`
      Right( Statement "c" (Increment (-20)) ("c", EqualTo, 10))
    "yrf dec -813 if jzm != 6" `shouldParseAs`
      Right( Statement "yrf" (Decrement (-813)) ("jzm", NotEqualTo, 6))

  describe "Interpreting" $ do
    it "adding to an empty map with a false predicate" $
      statementInterpreter Map.empty ( Statement "b" (Increment 5) ("a", GreaterThan, 1)) `shouldBe` Map.empty
    it "adding to an empty map with a true predicate" $
      statementInterpreter Map.empty ( Statement "b" (Increment 5) ("a", EqualTo, 0)) `shouldBe` fromList [("b",5)]
    it "adding to an empty map with a true predicate II" $
      statementInterpreter Map.empty ( Statement "b" (Increment 5) ("a", LessThan, 5)) `shouldBe` fromList [("b",5)]
    it "adding to an nonempty map with a true predicate" $
      statementInterpreter (fromList [("a",5)]) ( Statement "a" (Increment 5) ("a", GreaterThan, 0)) `shouldBe` fromList [("a",10)]


    it "adding to an nonempty map with a true predicate and a decrement statement" $
      statementInterpreter (fromList [("a",15)]) ( Statement "a" (Decrement (0)) ("a", GreaterThan, 0)) `shouldBe` fromList [("a",15)]

    it "adding to an nonempty map with a true predicate and a negative decrement statement" $
      statementInterpreter (fromList [("a",10)]) ( Statement "a" (Decrement (-15)) ("a", GreaterThan, 0)) `shouldBe` fromList [("a",25)]


    it "yrf dec -813 if jzm != 6" $
      statementInterpreter Map.empty( Statement "yrf" (Decrement (-813)) ("jzm", NotEqualTo, 6)) `shouldBe` fromList [("yrf", (813))]


  describe "Folding" $ do
    it "should return an empty map for an empty statement" $
      statementFolder [] `shouldBe` Right Map.empty
    it "should fold the first few examples sequentially" $
      statementFolder
        [
        "smi inc 781 if epx > -2",
        "yrf dec -813 if jzm != 6"
        -- ,"ben dec -383 if sp == 0"
        -- ,"tlj dec -356 if sp <= 4"
        -- ,"ssv dec -128 if tlj <= 360"
        ] `shouldBe`  Right (fromList
          [
          ("smi", 781)
          ,("yrf", 813)
          -- , ("jzm", 0)
          -- , ("ben", 383)
          -- , ("tlj", 356)
          -- , ("ssv", 128)
          ])

shouldParseAs stringToParse expectation=
  it stringToParse $
    parse statementParser "" stringToParse `shouldBe` expectation
