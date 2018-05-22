module Main where

import           Data.List        (find)
import           Data.Maybe       (catMaybes)
import           Data.Traversable
import           Lib
import           System.IO
import           Text.Parsec.Prim

main :: IO ()
main = do
  lns <-  lines  <$> readFile "./input"
  res <- pure $ do
    cols <-  traverse parseTheStuff lns
    maybeIndexedCols <- pure $ makeIndexedList $ (\c@( Column a b) -> (a, c)) <$> cols
    stuckColIndicies <- pure $ moveScanner [] 0 maybeIndexedCols
    stuckCols <- pure $ findColumnsByIndex cols stuckColIndicies
    severeties <- pure $ calculateSeverity  <$> (catMaybes stuckCols)
    pure $ sum severeties
  print res


parseTheStuff str = parse columnParser "" str
