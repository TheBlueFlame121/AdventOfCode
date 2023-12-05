module Main (main) where

import Text.Printf
import Data.List
import Data.Char

cartProd :: [a] -> [a] -> [[a]]
cartProd xs ys = [[x, y] | x <- xs, y <- ys]

getNumbers :: String -> [[(Int, Char)]]
getNumbers = filter (all (isDigit . snd)) . groupBy (\ a b -> isDigit (snd a) == isDigit (snd b)) . zip [0..]

getNeighbours :: Int -> Int -> Int -> [Int]
getNeighbours lineLen inputLen point = filter (/= currY*lineLen + currX) $ [y*lineLen + x | x <- validX, y <- validY]
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
solution1 input = sum . map (read . map snd) . filter hasNeighbourSymbol $ getNumbers input
  where hasNeighbourSymbol = any (any isSymbol . getNeighbours lineLen inpLen . fst)
        isSymbol = (\a -> not (isDigit a || a == '.')) . (input !!)
        lineLen = length ( takeWhile (/= '\n') input) +1
        inpLen = length input

numAndGear :: String -> [[Int]]
numAndGear input = concat [cartProd [number x] (gears x) | x <- getNumbers input]
  where number = read . map snd
        gears = nub . concatMap (filter (\a -> input!!a == '*') . getNeighbours lineLen inpLen . fst)
        lineLen = length (takeWhile (/= '\n') input) + 1
        inpLen = length input

solution2 :: String -> Int
solution2 = sum . map product
                      . filter (\a -> length a == 2)
                      . map (map head)
                      . groupBy (\a b -> last a == last b)
                      . sortBy (\ a b -> compare (last a) (last b))
                      . numAndGear

main :: IO ()
main = do
  input <- readFile "input"
  printf "Part1 : %d\n" $ solution1 input
  printf "Part2 : %d\n" $ solution2 input
