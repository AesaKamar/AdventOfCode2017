module Main where

import           Data.Traversable
import           Lib
import           System.IO
import           Text.Parsec.Prim

main :: IO ()
main = do
  lns <-  lines  <$> readFile "./input"
  -- Put this in a do stmt
  columns <- pure $ do
    parsed <-  traverse parseTheStuff lns
    indexedColumns <- pure $ (\c@( Column a b) -> (a, c)) <$> parsed
    pure $  makeIndexedList indexedColumns
  stuckColumns <- pure $ moveScanner [] 0 <$> columns
  pure ()



parseTheStuff str = parse columnParser "" str
