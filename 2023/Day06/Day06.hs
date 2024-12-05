module Main (main)where

import Data.List
import Data.Char
import Text.Printf

parse1:: [String] -> [(Int, Int)]
parse1 inp = zip times dists
    where times = head conv2ints
          dists = last conv2ints
          conv2ints = map (map (read::String->Int).tail.words) inp

parse2 :: [String] -> [(Int, Int)]
parse2 = (\[x, y] -> [(x, y)]) . map(read.concat.tail.words)

getRange :: (Int, Int) -> (Int, Int)
getRange (t, d) = (low_root, high_root)
    where low_root = floor $ (time - disc)/2 + 1
          high_root = ceiling $ (time + disc)/2 - 1
          disc = sqrt (time*time - 4*dist)
          time = fromIntegral t::Double
          dist = fromIntegral d::Double

solve :: ([String] -> [(Int, Int)]) -> [String] -> Int
solve parse_func = product . map((\(x, y) -> y-x+1) . getRange) . parse_func

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  printf "Part 1 : %d\n" $ solve parse1 input
  printf "Part 2 : %d\n" $ solve parse2 input
