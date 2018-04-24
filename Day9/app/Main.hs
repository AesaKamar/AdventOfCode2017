module Main where

import Lib
import System.IO
import Text.Parsec

-- ANSWER FOR PART 1
-- main :: IO ()
-- main = do
--   putStrLn $ replicate 15 '='
--   contents <- readFile "./input"
--   parsed <- return $ parse parseGroups "" contents
--   score <- return $ (scoreGroups 1) <$> parsed
--   putStrLn $ "Score is:" ++ show score
--   putStrLn $ replicate 15 '='
--   putStrLn $ "END!"
-- ANSWER FOR PART 2
main :: IO ()
main = do
  putStrLn $ replicate 15 '='
  contents <- readFile "./input"
  parsed <- return $ parse parseGroups "" contents
  score <- return $ countGarbage <$> parsed
  putStrLn $ "Score is:" ++ show score
  putStrLn $ replicate 15 '='
  putStrLn $ "END!"
