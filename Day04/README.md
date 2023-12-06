## Day 04
This challenge was easier than Day 02/03 and was a breath of fresh air. We are given a bunch of scratch cards with winning numbers and the drawn numbers. Part 1 asks us to find how many winning numbers we matched per card, calculate a scored based on this and find the sum of scores of all cards. The first function I wrote was to parse all the cards. I kept all the data, including the ID thinking it might be useful for part 2, but it wasn't. Here is the parse function:

```hs 
parseCards :: [String] -> [(Int, [[Int]])]
parseCards input = [(getID x, getNums y) | [x, y] <-  map (splitOn ": ") input]
  where getID = str2Int . last . splitOn " " 
        getNums = map (map str2Int . filter (not. null) . splitOn " ") . splitOn " | "
```

From this it is easy to check number of matches using the `isIn` function I wrote. The score calculation is also easy, both are given below:

```hs 
isIn :: Int -> [Int] -> Bool
isIn num [] = False
isIn num (x:xs) | num == x  = True
                | otherwise = isIn num xs

numOfWins :: [[Int]] -> Int
numOfWins nums = length . filter (`isIn` draws) $ wins
  where draws = last nums
        wins = head nums

wins2Score :: Int -> Int
wins2Score x | x==0 = 0 
             | otherwise = 2 ^ (x-1)
```

Using this, we can code up the `solution1`

```hs 
solution1 :: [String] -> Int
solution1 = sum . map (wins2Score . numOfWins . snd) . parseCards
```

For part2, then don't use the scoring. However, they change the "number" of cards of you have based on the number of matches in the current card. So say if I have a card with ID X, and it gets Y matches, then I get an additional each of the cards with IDs X+1, X+2, ..., X+Y. Now if I had Z copies of card with ID X, then I would get Z additional copies of each of the aforementioned card. We need to calculate how many total cards we get.

While the rules might seem quite complicated at first, they present a very easy recursive relation. We always start with the first card to see how many additional cards we get. The key thing to note is that nothing can affect the number of first cards. So once we are done processing the effect of the first card, we can remove it from the list and run the same logic again. 

To program this, I made a helper function called `increaseSubList` which increases the values of a continuous subportion of the list based on the parameters provided. It takes a starting Index, the length of the subportion, the amount to increment by and the list to return the modified list.

```hs 
increaseSubList :: Int -> Int -> Int -> [Int] -> [Int]
increaseSubList _ 0 _ list = list
increaseSubList startIdx len inc list = zipWith (\ x y
  -> (if x >= startIdx && x < startIdx + len then y + inc else y)) [0..] list
```

Using this, we can calculate the number of cards based on the number of matches. The function `findNumCards` does this by taking a list of "number of cards" and a list of "number of matches".

```hs 
findNumCards :: [Int] -> [Int] -> [Int]
findNumCards _ [] = []
findNumCards [] wins = findNumCards (replicate (length wins) 1) wins
findNumCards nums@(x:xs) wins@(y:ys) = x : findNumCards (increaseSubList 0 y x xs) ys
```

Now the final solution is straightforward as:
```hs
solution2 :: [String] -> Int
solution2 = sum . findNumCards [] . map (numOfWins . snd) . parseCards
```
