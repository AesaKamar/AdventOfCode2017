module Main where

import           Data.List        (find)
import           Data.Traversable
import           Lib
import           System.IO
import           Text.Parsec.Prim

main :: IO ()
main = do
  lns <-  lines  <$> readFile "./input"
  parsedCols <- pure $ traverse parseTheStuff lns
  eitherIndexedColumns <- pure $ do
    indexedColumns <- fmap (fmap (\c@( Column a b) -> (a, c))) parsedCols
    pure $  makeIndexedList indexedColumns
  stuckColumnIndicies <- pure $ moveScanner [] 0 <$> eitherIndexedColumns
  stuckColumns <- pure $ fmap(findColumnsByIndex <$> parsedCols) stuckColumnIndicies
  pure ()


parseTheStuff str = parse columnParser "" str
