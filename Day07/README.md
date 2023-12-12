## Day 07
This challenge wasn't a puzzle but mostly just a parsing problem. If we can represent the given data and constraints properly, the problem is trivial. I made extensive use of `data` and `type` in this challenge. I created a `type` `Hand` which contains the cards as a String and the bid as an Int. There is a `data` called `HandType` which basically acts as an enum. This contains the different classes of `Hand` and with the `deriving` keyword we can easily implement equality and ordering.

The last `data` is the `HandAttr` which contains properties related to the `Hand`. It contains the class `htype` which belongs to `HandType` and `hvalue` which is an Int representation of the string of cards. I also implemented `Ord` for `HandAttr` according to the given rules.

```hs 
type Hand =  (String, Int)

data HandType = High | One | Two | Three | Full | Four | Five
  deriving (Show, Eq, Ord)

data HandAttr = HandAttr { htype :: HandType, hvalue :: Int }
    deriving (Show, Eq)

instance Ord HandAttr where
  compare HandAttr {htype=htype1, hvalue=hval1} HandAttr {htype=htype2, hvalue=hval2}
          | htype1 == htype2 = compare hval1 hval2
          | otherwise = compare htype1 htype2
```

Now we just need helper functions to parse the given data into these types.

```hs
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
```

Note that I chose to stick with pattern matching for `getHandType2`. Another approach would be to simply generate all 13 possibilities when a joker is present and pick the maximum arising `HandType` from them. Lastly, we have the solve function 

```hs 
solve:: Int -> [String] -> Int
solve part = sum . zipWith (*) [1..] . map snd . sortBy (\x y -> compare (getHandAttr part x) (getHandAttr part y)) . parse
```
