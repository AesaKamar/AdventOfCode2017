module Main where
import           Control.Monad    (sequence)
import           Data.List
import           Lib
import           Test.Tasty
import           Test.Tasty.Hspec
import           Test.Tasty.HUnit
import           Text.Parsec


main = defaultMain $ testGroup "Day7" [ parsingTests, collapsingTests]

parsingTests :: TestTree
parsingTests = testGroup "Parsing individual stuff"
  [
  testCase "pbga (66)" $
      parse nameWeightAndSubtreesParser "" "pbga (66)" @?=
        Right ("pbga", 66, Nothing)
  ,

  testCase "ebii (61)" $
  parse nameWeightAndSubtreesParser "" "ebii (61)" @?=
    Right ("ebii", 61, Nothing)
  ,

  testCase "tknk (41) -> ugml, padx, fwft" $
  parse nameWeightAndSubtreesParser "" "tknk (41) -> ugml, padx, fwft " @?=
    Right ("tknk", 41, Just ["ugml", "padx", "fwft"])

  ,

  testCase " -> ugml, padx, fwft" $
  parse subTreeParser "" " -> ugml, padx, fwft"  @?=
    Right ["ugml", "padx", "fwft"]

  ]


treeEntries =
  [ ("qgcmjz", 87,  Just ["skzkx", "pzkofch"])
  , ("remwlv",  18, Nothing)
  , ("ngrmq",  80, Just ["cluej", "ywrxbgi", "saznyj"])
  , ("omqech",  119,  Just ["asqih", "chotekn"])
  ]

collapsingTests :: TestTree
collapsingTests = testGroup "Testing the collapsing function from parsed repr into tree data structure"
  [
  testCase "empty treeEntries" $
    collapseTrees ([], RootMultiTree []) @?= ([], RootMultiTree [])

  ,

  testCase "inserting a single entry" $
    case collapseTrees ([("remwlv",  18, Nothing)], RootMultiTree []) of
      (unsolved, tree) ->  assertBool (show unsolved) (null unsolved)

  ,

  testCase "inserting two unrelated trees" $
  case collapseTrees ([("remwlv",  18, Nothing), ("hello",  18, Nothing)], RootMultiTree []) of
    (unsolved, tree) ->  assertBool (show unsolved) $ case tree of
      RootMultiTree elements -> length elements == 2
      _                      -> False
  ]
