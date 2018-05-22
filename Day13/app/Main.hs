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
    stuckColAgg <- pure $
      let
        this = (\delay -> (moveScanner [] delay maybeIndexedCols)) <$> [0..10000000]
      in
        indexOfFirstEmpty this 0
    stuckCols <- pure $ findColumnsByIndex cols stuckColIndicies
    severeties <- pure $ calculateSeverity  <$> (catMaybes stuckCols)
    pure $ (sum severeties, stuckColAgg)
  print res


parseTheStuff str = parse columnParser "" str


indexOfFirstEmpty ::  [[a]] -> Integer -> Integer
indexOfFirstEmpty []  i      = 777
indexOfFirstEmpty ([] : _) i = i
indexOfFirstEmpty (a: as) i  = indexOfFirstEmpty  as (i + 1)
