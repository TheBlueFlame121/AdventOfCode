## Day 03
This day was very tough. I think the main reason for that was the lack of time I'm getting to put towards AoC. This is also evident in the [Day03_initial.hs](Day03_initial.hs). It is so badly written haha. I later cleaned up the code and made [Day03_final.hs](Day03_final.hs). Unfortunately due to this lack of time, I am falling behind :( . Anyway, let's talk about how we solved today's puzzle. I learned about tuples and the `groupBy` function in Haskell today.

We are given an ASCII grid which contains `.` as white space, numbers and symbols. The first task asks us find the sum of every number next to a symbol. The tricky part is that the numbers can be multiple digits long, and they occupy that many cells in the grid. I decided to keep the input as a string rather than dealing with something akin to a 2D array. I first wrote a function `getNumbers` which takes the input and returns a list of numbers in the grid as `[(Coord::Int, Digit::String)]`. I also wrote a rather simple getNeighbours function which takes coordinate as int and return a list of coordinates of the neighbors.

```hs 
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
```

After this, Part 1 was straightforward, we just filter to see if any of the neighbors is a symbol and then sum the numbers after recovering them using `map(read . map snd)`

```hs 
solution1 :: String -> Int
solution1 input = sum . map (read . map snd) . filter hasNeighbourSymbol $ getNumbers input
  where hasNeighbourSymbol = any (any isSymbol . getNeighbours lineLen inpLen . fst)
        isSymbol = (\a -> not (isDigit a || a == '.')) . (input !!)
        lineLen = length ( takeWhile (/= '\n') input) +1
        inpLen = length input
```

Part 2 is much more complicated. They say that a `gear` is any "*" that has exactly two numbers adjacent to it. Their product is called a ratio, and we need to find the sum of all ratios in the input. To approach this, I decided to create a list of all numbers which have a '*' in their neighbor as `[(num::Int, gearCoord::Int)]`. This is done by the function `numAndGear` shown below. The `cartProd` function just calculates the Cartesian product of two lists.

```hs 
cartProd :: [a] -> [a] -> [[a]]
cartProd xs ys = [[x, y] | x <- xs, y <- ys]

numAndGear :: String -> [[Int]]
numAndGear input = concat [cartProd [number x] (gears x) | x <- getNumbers input]
  where number = read . map snd
        gears = nub . concatMap (filter (\a -> input!!a == '*') . getNeighbours lineLen inpLen . fst)
        lineLen = length (takeWhile (/= '\n') input) + 1
        inpLen = length input
```

So first we find the indices of all gear which are neighbors to numbers. So we take the list of numbers and then convert them to neighbors before filtering them based on if they are a gear. `nub` just removes duplicated. I decided to do it this way because I wasn't sure if a number can be part of two gears. If a number is not next to a '*' symbol then we get an empty list and putting an empty list in `cartProd` returns an empty list anyway. Now we have a list of `(Number, GearCoord)`.

We can now sort this list based on `GearCoord` and then use `groupBy` to get the numbers adjacent to each '*' symbol. Now we just filter to make sure we keep only the lists of length two, find the product of each list and then sum them.

```hs 
solution2 :: String -> Int
solution2 = sum . map product
                      . filter (\a -> length a == 2)
                      . map (map head)
                      . groupBy (\a b -> last a == last b)
                      . sortBy (\ a b -> compare (last a) (last b))
                      . numAndGear
```
