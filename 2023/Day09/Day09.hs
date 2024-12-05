module Main (main)where

import Text.Printf

parseLine :: String -> [Int]
parseLine = map read . words

getDiffs :: [Int] -> [Int]
getDiffs [x] = []
getDiffs (x:y:xs) = (y-x) : getDiffs (y:xs)

getNext :: [Int] -> Int
getNext x | all (==0) x = 0
          | otherwise = last x + getNext (getDiffs x)

getPrev :: [Int] -> Int
getPrev x | all (==0) x = 0 
          | otherwise = head x - getPrev (getDiffs x)

solution1 :: [String] -> Int
solution1 = sum . map(getNext . parseLine)

solution2 :: [String] -> Int
solution2 = sum . map(getPrev . parseLine)

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  printf "Part 1 : %d\n" $ solution1 input
  printf "Part 2 : %d\n" $ solution2 input
