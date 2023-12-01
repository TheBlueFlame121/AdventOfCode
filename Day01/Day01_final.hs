module Main where

import Text.Printf
import Data.List
import Data.Char(isDigit, digitToInt)

f :: [Int] -> Int
f xs = head xs * 10 + last xs

parse1 :: [String] -> [[Int]]
parse1 = map (map digitToInt . filter isDigit)

parse2 :: [String] -> [[Int]]
parse2  = map internal
    where internal "" = []
          internal line@(x:xs)
            | "one"   `isPrefixOf` line = 1 : internal xs
            | "two"   `isPrefixOf` line = 2 : internal xs
            | "three" `isPrefixOf` line = 3 : internal xs
            | "four"  `isPrefixOf` line = 4 : internal xs
            | "five"  `isPrefixOf` line = 5 : internal xs
            | "six"   `isPrefixOf` line = 6 : internal xs
            | "seven" `isPrefixOf` line = 7 : internal xs
            | "eight" `isPrefixOf` line = 8 : internal xs
            | "nine"  `isPrefixOf` line = 9 : internal xs
            | "1"     `isPrefixOf` line = 1 : internal xs
            | "2"     `isPrefixOf` line = 2 : internal xs
            | "3"     `isPrefixOf` line = 3 : internal xs
            | "4"     `isPrefixOf` line = 4 : internal xs
            | "5"     `isPrefixOf` line = 5 : internal xs
            | "6"     `isPrefixOf` line = 6 : internal xs
            | "7"     `isPrefixOf` line = 7 : internal xs
            | "8"     `isPrefixOf` line = 8 : internal xs
            | "9"     `isPrefixOf` line = 9 : internal xs
            | otherwise = internal xs


solve :: ([String] -> [[Int]]) -> [String] -> Int
solve parse_func = sum . map f . parse_func

main :: IO()
main = do
  input <- lines <$> readFile "input"
  printf "Part 1: %d\n" $ solve parse1 input
  printf "Part 2: %d\n" $ solve parse2 input
