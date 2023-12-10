## Day 06
I don't understand these drastic fluctuations in difficulty. After yesterday being so difficult, this one was almost trivial. The input consists of times and distances for multiple races. For each race, you can spend some time at the start to increase your speed at 1 unit per second and then spend the rest of the time moving. We need to find how many different ways (increasing speed for different amount of time) we can win each race.

So for a race with time `t` and distance `d`, we need to find `x` such that `x*(t-x) > d`. This leads to a simple quadratic inequality. If we find the two roots `x0, x1`, then all values `x0, x0+1, ..., x1` will satisfy the inequality. We can find the roots using the following `getRange` function:

```hs
getRange :: (Int, Int) -> (Int, Int)
getRange (t, d) = (low_root, high_root)
    where low_root = floor $ (time - disc)/2 + 1
          high_root = ceiling $ (time + disc)/2 - 1
          disc = sqrt (time*time - 4*dist)
          time = fromIntegral t::Double
          dist = fromIntegral d::Double
```

Note that this method might get inaccurate at large inputs due to floating point computations. Part 1 asks for the product of number of ways for each race. So we just need a parse function `parse1` and a `solve` function:

```hs
parse1:: [String] -> [(Int, Int)]
parse1 inp = zip times dists
    where times = head conv2ints
          dists = last conv2ints
          conv2ints = map (map (read::String->Int).tail.words) inp

solve :: ([String] -> [(Int, Int)]) -> [String] -> Int
solve parse_func = product . map((\(x, y) -> y-x+1) . getRange) . parse_func
```

Part2 is also pretty simple. It says that instead of multiple times and distances being separated with whitespace, we just consider one race with time being all given times concatenated and the distance being all given distances concatenated. So to solve this part, all we need is a different parse function `parse2`:

```hs
parse2 :: [String] -> [(Int, Int)]
parse2 = (\[x, y] -> [(x, y)]) . map(read.concat.tail.words)
```
