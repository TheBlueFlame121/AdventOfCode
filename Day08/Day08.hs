module Main (main)where

import Text.Printf
import Data.Char
import qualified Data.Map as M
import Control.Exception.Base (assert)

parseLine :: String -> (String, (String, String))
parseLine = (\[x, y, z] -> (x, (y, z))) . filter (/="") . map (filter isAsciiUpper) . words

walkUntil :: (String -> Bool) -> M.Map String (String, String) -> String -> String -> Int -> (String, Int)
walkUntil condition graph instructions@(x:xs) node count = if condition node && count /= 0 then (node, count) else step
    where step = walkUntil condition graph xs (dirn $ graph M.! node) (count+1)
          dirn = if x == 'L' then fst else snd

solution1 :: M.Map String (String, String) -> String -> Int
solution1 graph instructions = snd $ walkUntil (=="ZZZ") graph (cycle instructions) "AAA" 0

solution2 :: M.Map String (String, String) -> String -> Int
solution2 graph instructions = foldl lcm 1 $ map (snd . (\x -> walkUntil (\y -> last y == 'Z') graph (cycle instructions) x 0)) startNodes
    where startNodes = filter (\x -> last x == 'A') . M.keys $ graph


main :: IO ()
main = do
  input <- lines <$> readFile "input"
  let instructions = head input
  let graph = M.fromList . map parseLine . drop 2 $ input

  printf "Part 1 : %d\n" $ solution1 graph instructions

  -- Some checks to make sure the solution for part 2 works
  -- They prove that the data is not general and has some special properties
  let finalNodes = map (\x -> walkUntil (\x -> last x == 'Z') graph (cycle instructions) x 0)
                   . filter (\x -> last x == 'A') . M.keys $ graph
  let check = assert (all ((==0) . (\x -> mod x (length instructions)) . snd) finalNodes)
              "Length of first node to final is exact multiple of instructions"
  let check = assert (all ((==0) . (\x -> mod x (length instructions)) . snd
              . (\x -> walkUntil (== x) graph (cycle instructions) x 0) . fst) finalNodes)
              "Length of Cycle from final node to itself is also multiple of instructions"
  -- It is only because of these properties that the LCM solution works
  -- I cannot think of a way to solve it for general case without simulation

  printf "Part 2 : %d\n" $ solution2 graph instructions
