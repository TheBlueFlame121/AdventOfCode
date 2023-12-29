## Day 08

The challenge today was straightforward as a problem but had a few oddities in the solution that I personally disliked. The challenge gives us a directed binary graph and asks us to traverse it. Along with the graph, we are also given a series of Left and Right instructions. Part 1 asks to find how many steps it would take to travel from the node "AAA" to the node "ZZZ" using the given instructions.

I represent the graph using a `Map` from `String` to `(String, String)` and I leave the instructions as a `String`. Here is how the parsing is handled:

```hs
import qualified Data.Map as M

parseLine :: String -> (String, (String, String))
parseLine = (\[x, y, z] -> (x, (y, z))) . filter (/="") . map (filter isAsciiUpper) . words

main = do
  input <- lines <$> readFile "input"
  let instructions = head input
  let graph = M.fromList . map parseLine . drop 2 $ input
```

I also wrote a helper function called `walkUntil` which counts the numbers of steps from a given starting node till a given condition is satisfied.

```hs
walkUntil :: (String -> Bool) -> M.Map String (String, String) -> String -> String -> Int -> (String, Int)
walkUntil condition graph instructions@(x:xs) node count = if condition node && count /= 0 then (node, count) else step
    where step = walkUntil condition graph xs (dirn $ graph M.! node) (count+1)
          dirn = if x == 'L' then fst else snd
```

Using these functions, we can simply write the solution for Part 1 as:

```hs
solution1 :: M.Map String (String, String) -> String -> Int
solution1 graph instructions = snd $ walkUntil (=="ZZZ") graph (cycle instructions) "AAA" 0
```

### Part 2 
This part is where my issues start. The challenge now is to start stepping multiple nodes together in parallel and only stop when all of them satisfy a given condition. The answer of this challenge is too big and hence simulation doesn't work. However, I can't think of any method other than simulation to solve this in the general case.

As it turns out, the input given to us is not general. Through experimenting in Python (easier than Haskell for me) I found out that the input has some special properties. I have included these checks in `main` function of the program for completeness.

```hs 
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
```

It's because of these special properties that I can just find the length of initial to final node for every possible starting node and then take LCM to find the solution for part 2.

```hs 
solution2 :: M.Map String (String, String) -> String -> Int
solution2 graph instructions = foldl lcm 1 $ map (snd . (\x -> walkUntil (\y -> last y == 'Z') graph (cycle instructions) x 0)) startNodes
    where startNodes = filter (\x -> last x == 'A') . M.keys $ graph
```

This gives us the solution for part 2 nicely.

Honestly, I really disliked the fact that it wasn't a programming solution but rather a hidden detail which solved this problem. I would've appreciated if the there was a way to solve this generally or if the hidden properties had been made clearer.
