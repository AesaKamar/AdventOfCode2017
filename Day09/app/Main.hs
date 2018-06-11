module Main where

import           Lib
import           System.IO
import           Text.Parsec

main :: IO ()
main = do
  putStrLn $ replicate 15 '='
  contents <- readFile "./input"
  parsed <- return $ parse parseGroups "" contents
  score <- return $ whichPartDoYouWannaSee <$> parsed
  putStrLn $ "Score is:" ++ show score
  putStrLn $ replicate 15 '='
  putStrLn $ "END!"

-- Modify this to switch between the two parts
whichPartDoYouWannaSee = part2Function

part1Function = (scoreGroups 1)

part2Function = countGarbage
