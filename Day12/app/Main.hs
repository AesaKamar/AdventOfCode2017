module Main where

import           Data.Graph
import           Data.Traversable (sequence)
import           Lib
import           System.IO
import           Text.Parsec
import           Text.Show.Pretty

main :: IO ()
main = do
  -- contents <- readFile "./sampleinput.txt"
  contents <- readFile "./day12input.txt"
  lns <- return $ lines contents
  fromTos <- return $ sequence $ fmap (parse parseLine "") lns
  edges <- return $ fmap (\x -> concat (normalize <$> x)) fromTos
  graph <- return $ fmap (buildG graphBound) edges
  numReachableFrom0 <- return $
    length <$> (\gr -> reachable gr 0) <$> graph
  numComponents <- return $
    length <$> (\gr -> components gr) <$> graph
  putStrLn $ ppShow numComponents
