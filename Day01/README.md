## Day 01
This challenge give us a bunch of alphanumeric strings in individual lines. We have to extract "calibration values" from these strings and return their sum. The calibration value in part 1 is the first appearing numerical digit * 10 plus the last appearing numerical digit.

My initial approach in [Day01\_initial.hs](Day01_initial.hs) was to extract all the first appearing digits into a list, extract all the last appearing digits into another list and then proceed with the calculation. However, this was not too elegant.

```hs
nums_extracted <- map (filter isDigit) . lines <$> readFile "input"
let msb = map ((read::String->Int) . take 1) nums_extracted
let lsb = map ((`mod` 10) . read::[Char]->Int) nums_extracted
print $ (10*sum msb) + sum lsb
```

The better approach (imo) that I employed in [Day01\_final.hs](Day01_final.hs) was to simply extract all digits and then do the calculation using head and last (first and last element in a list). This makes part 2 easier as well. Part 2 has the additional constraint that the digit might be spelled out like "one" instead of just a numerical "1".

The calculation using head and last is done by the function `f`. The `solve` function takes the input and a parse function to return the final value using `f`. The `parse1` uses `isDigit` to eliminate all non-numerical characters and then `digitToInt` converts a string of numerical characters to a list of Ints.

```hs 
f :: [Int] -> Int
f xs = head xs * 10 + last xs

solve :: ([String] -> [[Int]]) -> [String] -> Int
solve parse_func = sum . map f . parse_func

parse1 :: [String] -> [[Int]]
parse1 = map (map digitToInt . filter isDigit)
```

`parse2` on the other hand recursively matches the start of the string with the different cases and then does the same thing on the string but from the 2nd character onwards.

```hs 
parse2 :: [String] -> [[Int]]
parse2  = map internal
    where internal "" = []
          internal line@(x:xs)
            | "one"   `isPrefixOf` line = 1 : internal xs
            | "two"   `isPrefixOf` line = 2 : internal xs
            | "three" `isPrefixOf` line = 3 : internal xs
            | "four"  `isPrefixOf` line = 4 : internal xs
            | "five"  `isPrefixOf` line = 5 : internal xs
            | "six"   `isPrefixOf` line = 6 : internal xs
            | "seven" `isPrefixOf` line = 7 : internal xs
            | "eight" `isPrefixOf` line = 8 : internal xs
            | "nine"  `isPrefixOf` line = 9 : internal xs
            | "1"     `isPrefixOf` line = 1 : internal xs
            | "2"     `isPrefixOf` line = 2 : internal xs
            | "3"     `isPrefixOf` line = 3 : internal xs
            | "4"     `isPrefixOf` line = 4 : internal xs
            | "5"     `isPrefixOf` line = 5 : internal xs
            | "6"     `isPrefixOf` line = 6 : internal xs
            | "7"     `isPrefixOf` line = 7 : internal xs
            | "8"     `isPrefixOf` line = 8 : internal xs
            | "9"     `isPrefixOf` line = 9 : internal xs
            | otherwise = internal xs
```

The advantage of making everything stand alone and modular like this (I think the term haskell-ers use is "Pure") is that the main is very simple.

```hs 
main :: IO()
main = do
  input <- lines <$> readFile "input"
  printf "Part 1: %d\n" $ solve parse1 input
  printf "Part 2: %d\n" $ solve parse2 input
```
