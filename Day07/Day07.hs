module Main (main)where

import Text.Printf
import Data.List
import Data.Char
import Data.Maybe

type Hand =  (String, Int)

data HandType = High | One | Two | Three | Full | Four | Five
  deriving (Show, Eq, Ord)

cards1 = ['2', '3', '4', '5', '6', '7', '8', '9', 'T', 'J', 'Q', 'K', 'A']
cards2 = ['J', '2', '3', '4', '5', '6', '7', '8', '9', 'T', 'Q', 'K', 'A']


data HandAttr = HandAttr { htype :: HandType, hvalue :: Int }
    deriving (Show, Eq)

instance Ord HandAttr where
  compare HandAttr {htype=htype1, hvalue=hval1} HandAttr {htype=htype2, hvalue=hval2}
          | htype1 == htype2 = compare hval1 hval2
          | otherwise = compare htype1 htype2

getHandType1 :: String -> HandType
getHandType1 input = case (sort . map length . group . sort) input of
      [5]           -> Five
      [1, 4]        -> Four
      [2, 3]        -> Full
      [1, 1, 3]     -> Three
      [1, 2, 2]     -> Two
      [1, 1, 1, 2]  -> One
      _             -> High

getHandType2:: String -> HandType
getHandType2 input = case (sort . map length . group . sort. filter (/='J')) input of
    c
      | c `elem` [[], [1], [2], [3], [4], [5]]            -> Five
      | c `elem` [[1,1], [1,2], [1, 3], [1, 4]]       -> Four
      | c `elem` [[2, 2], [2, 3]]                     -> Full
      | c `elem` [[1, 1, 1], [1, 1, 2], [1, 1, 3]]    -> Three
      | c ==     [1, 2, 2]                            -> Two
      | c `elem` [[1, 1, 1, 1], [1, 1, 1, 2]]         -> One
      | otherwise                                     -> High

getHandVal :: String -> String -> Int
getHandVal cards = foldl' (\x y -> x*length cards + fromJust (elemIndex y cards)) 0

getHandAttr :: Int -> Hand -> HandAttr
getHandAttr part (s, _) = case part of
        1 -> HandAttr {htype=getHandType1 s, hvalue=getHandVal cards1 s}
        2 -> HandAttr {htype=getHandType2 s, hvalue=getHandVal cards2 s}


parse :: [String] -> [Hand]
parse = map ((\[x, y] -> (x, read y)) . words)

solve:: Int -> [String] -> Int
solve part = sum . zipWith (*) [1..] . map snd . sortBy (\x y -> compare (getHandAttr part x) (getHandAttr part y)) . parse

main :: IO ()
main = do
  input <- lines <$> readFile "input"
  printf "Part 1 : %d\n" $ solve 1 input
  printf "Part 2 : %d\n" $ solve 2 input
