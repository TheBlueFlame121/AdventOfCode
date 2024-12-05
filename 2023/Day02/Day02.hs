module Main where

import Text.Printf
import Data.List
import Data.Maybe
import Data.Char(isDigit, digitToInt)


str2Int = read::String -> Int

findString :: (Eq a) => [a] -> [a] -> Int
findString search str = fromMaybe (-1) $ findIndex (isPrefixOf search) (tails str)

splitOn :: String -> String -> [String]
splitOn delim "" = []
splitOn delim str = case findString delim str of
                        -1 -> [str]
                        x -> take x str : splitOn delim (drop (x + length delim) str)

parseDraws :: String -> [[(Int, String)]]
parseDraws =
  map (map ((\[x, y] -> (str2Int x,y)) . words) . splitOn ", ") . splitOn "; "


parseLine:: String -> (Int, [[(Int, String)]])
parseLine = (\[x, y] -> (str2Int . last . words $ x, parseDraws y)) . splitOn ": "

drawPossible:: (Int, String) -> Bool
drawPossible (num, color) = case color of
                            "red" -> num <= 12
                            "green" -> num <= 13
                            "blue" -> num <= 14
                            _ -> False

gamePossible:: (Int, [[(Int, String)]]) -> Int
gamePossible (id, details) = if all (all drawPossible) details then id else 0

draw2Vec:: (Int, String) -> (Int, Int, Int)
draw2Vec (num, color) = case color of
                            "red" -> (num, 0, 0)
                            "green" -> (0, num, 0)
                            "blue" -> (0, 0, num)
                            _ -> (0, 0, 0)

combineVec:: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
combineVec (a, b, c) (x, y, z) = (max a x, max b y, max c z)

powerSet :: [(Int, Int, Int)] -> (Int, Int, Int)
powerSet = foldl' combineVec (0, 0, 0)

gamePower:: (Int, [[(Int, String)]]) -> Int
gamePower (id, details) = (\(x, y, z) -> x*y*z) . powerSet $ map (powerSet . map draw2Vec) details

solve:: ((Int, [[(Int, String)]]) -> Int) -> [String] -> Int
solve gameFunc = sum . map (gameFunc . parseLine)

main :: IO()
main = do
  input <- lines <$> readFile "input"
  printf "Part 1 = %d\n" $ solve gamePossible input
  printf "Part 2 = %d\n" $ solve gamePower input
