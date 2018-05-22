module Main where

import           Control.Applicative ((<**>))
import           Data.Foldable       (any)
import           Data.List           (find)
import           Data.Maybe          (catMaybes)
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
    firstUncaughtRun <- pure $
      let runWithDelay delay = moveScanner [] delay maybeIndexedCols
          colSizes = (\(Column i s) -> s) <$> cols
          -- divisibilityPredicates =  isDivisibleBy <$> colSizes
          -- delaysToSearch = filter (anyfilt divisibilityPredicates) [0..]
          delaysToSearch = [0..]
      in indexOfFirstEmpty (runWithDelay <$> delaysToSearch) 0
    stuckCols <- pure $ findColumnsByIndex cols stuckColIndicies
    severeties <- pure $ calculateSeverity  <$> (catMaybes stuckCols)
    pure $ (sum severeties, firstUncaughtRun)
  print res


parseTheStuff str = parse columnParser "" str


indexOfFirstEmpty ::  [[a]] -> Integer -> Maybe Integer
indexOfFirstEmpty []  i      = Nothing
indexOfFirstEmpty ([] : _) i = Just i
indexOfFirstEmpty (a: as) i  = indexOfFirstEmpty  as (i + 1)


isDivisibleBy x = (/= 0) . (`mod` x)


anyfilt :: [(a -> Bool)] -> (a -> Bool)
anyfilt fns = \el -> any (\fn -> fn el) fns

allfilt :: [(a -> Bool)] -> (a -> Bool)
allfilt fns = \el -> all (\fn -> fn el) fns
