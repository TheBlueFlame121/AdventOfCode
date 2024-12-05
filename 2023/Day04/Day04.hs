module Main (main) where

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

isIn :: Int -> [Int] -> Bool
isIn num [] = False
isIn num (x:xs) | num == x  = True
                | otherwise = isIn num xs

parseCards :: [String] -> [(Int, [[Int]])]
parseCards input = [(getID x, getNums y) | [x, y] <-  map (splitOn ": ") input]
  where getID = str2Int . last . splitOn " " 
        getNums = map (map str2Int . filter (not. null) . splitOn " ") . splitOn " | "

numOfWins :: [[Int]] -> Int
numOfWins nums = length . filter (`isIn` draws) $ wins
  where draws = last nums
        wins = head nums

wins2Score :: Int -> Int
wins2Score x | x==0 = 0 
             | otherwise = 2 ^ (x-1)

increaseSubList :: Int -> Int -> Int -> [Int] -> [Int]
increaseSubList _ 0 _ list = list
increaseSubList startIdx len inc list = zipWith (\ x y
  -> (if x >= startIdx && x < startIdx + len then y + inc else y)) [0..] list

findNumCards :: [Int] -> [Int] -> [Int]
findNumCards _ [] = []
findNumCards [] wins = findNumCards (replicate (length wins) 1) wins
findNumCards nums@(x:xs) wins@(y:ys) = x : findNumCards (increaseSubList 0 y x xs) ys

solution1 :: [String] -> Int
solution1 = sum . map (wins2Score . numOfWins . snd) . parseCards

solution2 :: [String] -> Int
solution2 = sum . findNumCards [] . map (numOfWins . snd) . parseCards

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  printf "Part 1 : %d\n" $ solution1 input
  printf "Part 2 : %d\n" $ solution2 input
