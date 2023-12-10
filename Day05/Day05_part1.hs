module Main (main)where

import Text.Printf
import Data.List
import Data.Maybe

str2Int = read::String -> Int

findString :: (Eq a) => [a] -> [a] -> Int
findString search str = fromMaybe (-1) $ findIndex (isPrefixOf search) (tails str)

splitOn :: String -> String -> [String]
splitOn delim "" = []
splitOn delim str = case findString delim str of
                        -1 -> [str]
                        x -> take x str : splitOn delim (drop (x + length delim) str)

type Map = [(Int, Int, Int)]

getSeeds :: [String] -> [Int]
getSeeds = map read . tail . splitOn " ". head

getMap :: String -> Map
getMap input = sortBy (\ (a, b, c) (x, y, z) -> compare y b) $ [(read a, read b, read c) | [a, b, c] <- map words . tail . splitOn "\n" $ input]

applyMap :: Map -> Int -> Int
applyMap [] val = val
applyMap map@(x:xs) val | val >= b = if val < b+c then val - b + a else val
                        | otherwise  = applyMap xs val
  where (a, b, c) = x

applyMaps :: [Map] -> Int -> Int
applyMaps xs val = foldl (flip applyMap) val xs

solution1 :: [String] -> [Int] -> Int
solution1 maps = minimum . map (applyMaps (map getMap maps))

main :: IO ()
main = do
  input <- splitOn "\n\n" <$> readFile "input"
  let seeds = getSeeds input
  printf "Part 1 : %d\n" $ solution1 (tail input) seeds
