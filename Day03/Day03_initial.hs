module Main (main) where

import Text.Printf
import Data.List
import Data.Char

getNumbers :: String -> [[(Int, Char)]]
getNumbers = filter (all (isDigit . snd)) . groupBy (\ a b -> isDigit (snd a) == isDigit (snd b)) . zip [0..]

getNeighbours :: Int -> Int -> Int -> [Int]
getNeighbours lineLen inputLen point = filter (\a -> a /= currY*lineLen + currX) $ [y*lineLen + x | x <- validX, y <- validY]
      where validX  | currX == 0 = [currX, currX + 1]
                    | currX == lineLen - 2 = [currX - 1, currX]
                    | otherwise = [currX - 1, currX, currX+1]
            validY  | currY == 0 = [currY, currY + 1]
                    | currY == maxY - 1 = [currY - 1, currY]
                    | otherwise = [currY - 1, currY, currY+1]
            currX = point `mod` lineLen
            currY = point `div` lineLen
            maxY = inputLen `div` lineLen


solution1 :: String -> Int
solution1 input = sum . map ((read::String->Int) . map snd) . filter (any (any ((\a -> not (isDigit a || a == '.')) . (input !!)) . getNeighbours (length ( takeWhile (/= '\n') input) +1) (length input) . fst)) $ getNumbers input

numsAdjToGears :: String -> [[(Int, Char)]]
numsAdjToGears input = filter (any (any (\a -> input!!a == '*') . getNeighbours (length ( takeWhile (/= '\n') input) +1) (length input) . fst)) $ getNumbers input

getGears :: String -> [[Int]]
getGears input = map (nub . concatMap (filter (\a -> input!!a == '*'). getNeighbours (length ( takeWhile (/= '\n') input) +1) (length input) . fst)) . numsAdjToGears $ input

cartProd :: [a] -> [a] -> [[a]]
cartProd xs ys = [[x, y] | x <- xs, y <- ys]

numAndGear :: [[(Int, Char)]] -> [[Int]] -> [[Int]]
numAndGear nums gears = sortBy (\ a b -> compare (last a) (last b)) $ concat [cartProd [x] y | (x, y) <- zip temp gears]
  where temp = map ((read::String->Int) . map snd) nums

finalAns2 :: [[Int]] -> Int
finalAns2 = sum . map product . filter (\a -> length a == 2) . map (map head) . groupBy (\a b -> last a == last b)

solution2 :: String -> Int
solution2 input = finalAns2 $ numAndGear (numsAdjToGears input) (getGears input)

main :: IO ()
main = do
  input <- readFile "input"
  printf "Part1 : %d\n" $ solution1 input
  printf "Part2 : %d\n" $ solution2 input
