module Main (main)where

import Text.Printf
import Prelude hiding (Right, Left)
import Data.List
import Data.Maybe
import Data.Char

findString :: (Eq a) => [a] -> [a] -> Int
findString search str = fromMaybe (-1) $ findIndex (isPrefixOf search) (tails str)

intersect :: (Eq a) => [a] -> [a] -> [a]
intersect [] = const []
intersect xs = filter (`elem` xs)

data Direction = Up | Down | Left | Right
  deriving (Show, Eq)

getNextInDirn :: Int -> Int -> Direction -> Int
getNextInDirn lineLen point dir = case dir of
    Up -> point - lineLen
    Down -> point + lineLen
    Left -> point - 1
    Right -> point + 1

findStart :: String -> Int
findStart = findString "S"

findStartDirn :: String -> Int -> Direction
findStartDirn inp point | (inp!!getNextInDirn lineLen point Up) `elem` "|7F" = Up
                        | (inp!!getNextInDirn lineLen point Down) `elem` "|LJ" = Down
                        | (inp!!getNextInDirn lineLen point Left) `elem` "-FL" = Left
                        | (inp!!getNextInDirn lineLen point Right) `elem` "-J7" = Right
  where lineLen = findString "\n" inp + 1

getOutDirn :: Direction -> Char -> Direction
getOutDirn inDir pipe = case (inDir, pipe) of
    (Up, '|')     -> Up
    (Up, 'F')     -> Right
    (Up, '7')     -> Left
    (Down, '|')   -> Down
    (Down, 'L')   -> Right
    (Down, 'J')   -> Left
    (Right, '-')  -> Right
    (Right, '7')  -> Down
    (Right, 'J')  -> Up
    (Left, '-')   -> Left
    (Left, 'F')   -> Down
    (Left, 'L')   -> Up

walkLoop :: String -> Int -> Direction -> [Int] -> [Int]
walkLoop grid point inDir loopSeen | grid!!point == 'S' = case loopSeen of
                                                          [] -> walkLoop grid (getNextInDirn lineLen point startDir) startDir [point]
                                                          _ -> loopSeen
                                   | otherwise          = walkLoop grid (getNextInDirn lineLen point outDir) outDir [point]++loopSeen
                                      where outDir = getOutDirn inDir (grid!!point)
                                            startDir = findStartDirn grid point
                                            lineLen = findString "\n" grid + 1

solution1 :: String -> Int
solution1 input = div (length $ walkLoop input (findStart input) Down [])  2 

pairDiff :: [Int] -> Int
pairDiff [] = 0
pairDiff [x] = 0
pairDiff (x:y:xs) = y-x-1 + pairDiff xs

solution2 :: String -> Int
solution2 input = sum . map(pairDiff . sort .map fst) . groupBy(\(x, y) (a, b) -> y == b) .sortBy(\(x, y) (a, b) -> compare y b) . map(\x -> (mod x lineLen, div x lineLen)) $ loop
  where loop = walkLoop input (findString "S" input) Down []
        lineLen = findString "\n" input + 1

main :: IO ()
main = do
  input <- readFile "input"
  printf "Part 1 : %d\n" $ solution1 input
