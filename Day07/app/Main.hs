module Main where

import           Data.Functor
import           Lib
import           Text.Parsec

main :: IO ()
main =
  let inputLinesIO = readLines "./app/input"
      somth =
        fmap (\ils -> parse nameWeightAndSubtreesParser "" <$> ils) inputLinesIO
  in do res <- somth
        print res
