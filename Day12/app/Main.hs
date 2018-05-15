module Main where

import           Data.Graph
import           Data.Traversable (sequence)
import           Lib
import           System.IO
import           Text.Parsec

main :: IO ()
main = do
  contents <- readFile "./day12input.txt"
  lns <- return $ lines contents
  fromTos <- return $ sequence $ fmap (parse parseLine "") lns
  edges <- return $ fmap (\x -> concat (normalize <$> x)) fromTos
  graph <- return $ fmap (buildG graphBound) edges
  reachableFrom0 <- return $  (\gr -> reachable gr 0) <$> graph
  putStrLn $ show reachableFrom0
