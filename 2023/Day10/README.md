## Day 10
This was the last challenge I was able to attempt before AOC 2023 came to a close. I was only able to solve Part 1 and not Part 2. I think this was partly because of how much I complicated part 1.

That challenge gave us an ASCII map art showing a bunch of pipes. The map has one special character "S" which marks the start of the main loop. The main loop is a network of pipes that connect to each other to form a close loop and Part 1 asks that we output half the length of this loop.

I wrote some helper functions `findStart`, `findStartDirn`, `getNextInDirn` and `getOutDirn`. They are all pretty self-explanatory I feel.

```hs 
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
```

Then I wrote a function `walkLoop`. This function walks the loop and then collects all the pipes that are a part of the loop. In hindsight, I didn't need to collect the loop, but I could've just kept the length of the part seen.

```hs 
walkLoop :: String -> Int -> Direction -> [Int] -> [Int]
walkLoop grid point inDir loopSeen | grid!!point == 'S' = case loopSeen of
                                                          [] -> walkLoop grid (getNextInDirn lineLen point startDir) startDir [point]
                                                          _ -> loopSeen
                                   | otherwise          = walkLoop grid (getNextInDirn lineLen point outDir) outDir [point]++loopSeen
                                      where outDir = getOutDirn inDir (grid!!point)
                                            startDir = findStartDirn grid point
                                            lineLen = findString "\n" grid + 1
```

Then `solution1` just need to take the length of the output from `walkLoop` and divide it by 2.

```hs 
solution1 :: String -> Int
solution1 input = div (length $ walkLoop input (findStart input) Down [])  2
```

For part2, the challenge asks us to count the number of tiles which lie on the "inside" of the loop. I could not find a solution to this problem, but I do have a couple of ideas for when I pick this repo up again in the future.
