module Main where

import Data.List
import Data.Char(isDigit)

main :: IO()
main = do
  nums_extracted <- map (filter isDigit) . lines <$> readFile "input"
  let msb = map ((read::String->Int) . take 1) nums_extracted
  let lsb = map ((`mod` 10) . read::[Char]->Int) nums_extracted
  print $ (10*sum msb) + sum lsb
