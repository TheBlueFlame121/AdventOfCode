## Day 09
This was one of the simplest challenges of all. We are given a series of numbers, and we have to predict the next number. This is done by taking the difference of each term and predicting the next number in that series which gives us a nice recursive way to find the next term.

I start by writing the `parseLine` and `getDiffs` function:

```hs
parseLine :: String -> [Int]
parseLine = map read . words

getDiffs :: [Int] -> [Int]
getDiffs [x] = []
getDiffs (x:y:xs) = (y-x) : getDiffs (y:xs)
```

Now we can write the `getNext` functions and `solution1`

```hs
getNext :: [Int] -> Int
getNext x | all (==0) x = 0
          | otherwise = last x + getNext (getDiffs x)

solution1 :: [String] -> Int
solution1 = sum . map(getNext . parseLine)
```

Part 2 asks us find the previous term instead of the next one, we write a function for `getPrev` and then write `solution2`

```hs
getPrev :: [Int] -> Int
getPrev x | all (==0) x = 0 
          | otherwise = head x - getPrev (getDiffs x)

solution2 :: [String] -> Int
solution2 = sum . map(getPrev . parseLine)
```

Nice and easy problem overall. Didn't even have to think much as the challenge description directly gave us the solution.
