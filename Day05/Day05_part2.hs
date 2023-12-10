module Main (main)where

import Text.Printf
import Data.List
import Data.Maybe
import Data.Time.Calendar.Easter (sundayAfter)
import Distribution.Simple (intersectVersionRanges)

str2Int = read::String -> Int

findString :: (Eq a) => [a] -> [a] -> Int
findString search str = fromMaybe (-1) $ findIndex (isPrefixOf search) (tails str)

splitOn :: String -> String -> [String]
splitOn delim "" = []
splitOn delim str = case findString delim str of
                        -1 -> [str]
                        x -> take x str : splitOn delim (drop (x + length delim) str)

data Map = Map {to::Int, from::Int, len::Int}
    deriving (Eq, Show)
data Range = Range { start :: Int, end :: Int}
    deriving (Eq, Show)

getSeeds :: [String] -> [Int]
getSeeds = map read . tail . splitOn " ". head

getSeedRanges :: [Int] -> [Range]
getSeedRanges [] = []
getSeedRanges (x:y:res)  = Range x (x+y) : getSeedRanges res

getSeedRanges1 :: [Int] -> [Range]
getSeedRanges1 [] = []
getSeedRanges1 (x:res)  = Range x (x+1) : getSeedRanges1 res

getMap :: String -> [Map]
getMap input = [Map (read a) (read b) (read c) | [a, b, c] <- map words . tail . splitOn "\n" $ input]

applyClean :: Range -> Map -> Range
applyClean inp@Range{start=s, end=e} map@Map{to=a, from=b, len=c} = Range (s-b+a) (e-b+a)

applyMap :: Range -> [Map] -> [Range]
applyMap inp [] = [inp]
applyMap inp (m@Map{to=a, from=b, len=c}:ms) = concatMap (tryApply m) intersections
      where tryApply map dat  | isIn (start dat) (from m, len m)  = [applyClean dat map]
                              | otherwise = applyMap dat ms
            isIn s (b, c) = s >= b && s < b+c
            intersections = splits inp (Range b (b+c))

applyMaps :: [Range] -> [[Map]] -> [Range]
applyMaps dat [] = dat
applyMaps dat@(d:ds) maps@(m:ms) = applyMaps (concatMap (`applyMap` m) dat) ms

-- Return before, intersection and after 
-- Can be used for applying maps one by one
splits :: Range -> Range -> [Range]
splits d@Range{start = s, end = e} m@Range{start = s', end = e'}
  | s > e' = [d]
  | e < s' = [d]
  | s < s' = Range s s' : if e <= e' then [Range s' e] else [Range s' e', Range e' e]
  | s <= e' = if e <= e' then [Range s e] else [Range s e', Range e' e]

solution2 :: [Range] -> [[Map]] -> Int
solution2 seeds maps = start. minimumBy (\a b -> compare (start a) (start b)) $ applyMaps seeds maps

main :: IO ()
main = do
  input <- splitOn "\n\n" <$> readFile "input"
  let seeds = getSeeds input
  let maps = map getMap (tail input)
  printf "Part 1 : %d\n" $ solution2 (getSeedRanges1 seeds) maps
  printf "Part 2 : %d\n" $ solution2 (getSeedRanges seeds) maps
