## Day 02
This challenge was so much harder than day 1. The main hurdle was in parsing and representing the data. This lead to my solution containing very weird function signatures and a lot of functions thrown about. I probably could've looked up how to define custom data types and operations to tidy things up better, but that's for another time. On the plus side, I had to write my own `splitOn` function which I can see being very useful in the other days and I learned about lambda functions.

```hs
findString :: (Eq a) => [a] -> [a] -> Int
findString search str = fromMaybe (-1) $ findIndex (isPrefixOf search) (tails str)

splitOn :: String -> String -> [String]
splitOn delim "" = []
splitOn delim str = case findString delim str of
                        -1 -> [str]
                        x -> take x str : splitOn delim (drop (x + length delim) str)
```

The input is a series of games given one per line, each with a unique ID. The game consists of drawing a random numbers cubes from a bag a few times per game. The cubes can be colored either red, green or blue. Part 1 asks to find games which were possible given 12 red, 13 greed and 14 blue cubes in the bag. We have to return the sum of the IDs of these games.

The functions `parseLine` and `parseDraws` convert the given string into a series of nested tuples and lists using `splitOn` and some lambda functions. Then the function `drawPossible` checks if the balls drawn are less than the limit and the function `gamePossible` just applies this over all draws. If the checks pass then the ID is returned otherwise 0. Summing all these values gives us the solution.

```hs
parseDraws :: String -> [[(Int, String)]]
parseDraws =
  map (map ((\[x, y] -> (str2Int x,y)) . words) . splitOn ", ") . splitOn "; "


parseLine:: String -> (Int, [[(Int, String)]])
parseLine = (\[x, y] -> (str2Int . last . words $ x, parseDraws y)) . splitOn ": "

drawPossible:: (Int, String) -> Bool
drawPossible (num, color) = case color of
                            "red" -> num <= 12
                            "green" -> num <= 13
                            "blue" -> num <= 14
                            _ -> False

gamePossible:: (Int, [[(Int, String)]]) -> Int
gamePossible (id, details) = if all (all drawPossible) details then id else 0
```

For part 2, we are supposed to find the "power" value for each game. Simply put, if x, y and z are the minimum number of red, green and blue balls required to make the game possible then the power value is x*y*z. To keep a track of separate value per color, we convert each draw into a tuple of 3 Ints using `draw2Vec`. Then these tuples are combined using `combineVec` and `powerSet`. The function `powerSet` just applies `combineVec` to a list, just like `sum` applied `+` to a list. We use these functions find the power set of each game then use `gamePower` to calculate the final value.

```hs
combineVec:: (Int, Int, Int) -> (Int, Int, Int) -> (Int, Int, Int)
combineVec (a, b, c) (x, y, z) = (max a x, max b y, max c z)

powerSet :: [(Int, Int, Int)] -> (Int, Int, Int)
powerSet = foldl' combineVec (0, 0, 0)

gamePower:: (Int, [[(Int, String)]]) -> Int
gamePower (id, details) = (\(x, y, z) -> x*y*z) . powerSet $ map (powerSet . map draw2Vec) details
```

Everything is finally tied together using the solve function

```hs
solve:: ((Int, [[(Int, String)]]) -> Int) -> [String] -> Int
solve gameFunc = sum . map (gameFunc . parseLine)

main :: IO()
main = do
  input <- lines <$> readFile "input"
  printf "Part 1 = %d\n" $ solve gamePossible input
  printf "Part 2 = %d\n" $ solve gamePower input
```
