## Day 05
### Part 1
This challenge was an extreme spike in the difficulty. It actually took me multiple sessions to figure this one out. The input consists of seeds which are integers and a bunch of transforms. Each transform contains multiple maps of the form `(to::Int, from::Int, len::Int)`. A map can be understood as saying the values `from, from+1, ..., from+len-1` map to `to, to+1, ..., to+len-1` respectively. The maps within each transform don't overlap and if a value is not present in any of the maps then it gets transformed to itself.

So for part 1, They want us to find the minimum value after all the transforms have been applied. I start by defining a type `Map` which is a "transform" in the explanation above. Then I write functions to parse everything.

```hs 
type Map = [(Int, Int, Int)]

getSeeds :: [String] -> [Int]
getSeeds = map read . tail . splitOn " ". head

getMap :: String -> Map
getMap input = sortBy (\ (a, b, c) (x, y, z) -> compare y b) $ [(read a, read b, read c) | [a, b, c] <- map words . tail . splitOn "\n" $ input]
```

Notice that I sort the maps within transform by the start of their source ranges in descending order. This makes applying a transform easier as we don't need to search the entire transform to find which map applies to a particular value. Now we can write `applyMap` by going through the list and seeing which map is applicable to our input. We can also write `applyMaps` which applies multiples maps to an input.

```hs
applyMap :: Map -> Int -> Int
applyMap [] val = val
applyMap map@(x:xs) val | val >= b = if val < b+c then val - b + a else val
                        | otherwise  = applyMap xs val
  where (a, b, c) = x

applyMaps :: [Map] -> Int -> Int
applyMaps xs val = foldl (flip applyMap) val xs
```

The flip is there because the order of transforms from parsing is reverse of order of applying the transforms. Now solution1 is straightforward:

```hs
solution1 :: [String] -> [Int] -> Int
solution1 maps = minimum . map (applyMaps (map getMap maps))

main :: IO ()
main = do
  input <- splitOn "\n\n" <$> readFile "input"
  let seeds = getSeeds input
  printf "Part 1 : %d\n" $ solution1 (tail input) seeds
```

This can be found in [Day05\_part1.hs](Day05_part1.hs)

### Part 2
This is where everything goes crazy. Previously seeds were given to us as just a bunch of integers. Now they claim that the seeds are to be considered two at a time defining a range start and range length. This means that they define ranges as `x y` going to `x, x+1, ..., x+y-1`. This increases the number of data that we need to transform by a lot. It is no longer feasible to keep track of every single value as it would take too long to run. 

First we write a function to parse seeds as ranges. Since the types are going to get quite complicated, we defined custom structs using `data`. I keep map as the same as defined in the problem, but I convert range to be a start and an end value where the end value is not included in the range.

```hs
data Map = Map {to::Int, from::Int, len::Int}
    deriving (Eq, Show)
data Range = Range { start :: Int, end :: Int}
    deriving (Eq, Show)

getSeedRanges :: [Int] -> [Range]
getSeedRanges [] = []
getSeedRanges (x:y:res)  = Range x (x+y) : getSeedRanges res
```

The solution to ensure the program finishes in time is to apply the transforms on ranges. However, this presents new challenges. Our input ranges will not always match perfectly with a given map. So we need to find a way to split a source range into 3 parts: range before map, range intersecting with map and range after map. This split function will convert a range into a list of ranges which presents difficulties writing an apply function as well.

We start by defining the `splits` function which takes two ranges and outputs a list of ranges. The first range is the one we want to split according to the second range.

```hs
splits :: Range -> Range -> [Range]
splits d@Range{start = s, end = e} m@Range{start = s', end = e'}
  | s > e' = [d]
  | e < s' = [d]
  | s < s' = Range s s' : if e <= e' then [Range s' e] else [Range s' e', Range e' e]
  | s <= e' = if e <= e' then [Range s e] else [Range s e', Range e' e]
```

Let us also define a `applyMapClean` function which assumes the input range inside the map.

```hs
applyClean :: Range -> Map -> Range
applyClean inp@Range{start=s, end=e} map@Map{to=a, from=b, len=c} = Range (s-b+a) (e-b+a)
```

Now we can define a general `applyMap` function. This takes an input range, a transform as list of maps and output a list of ranges. We do this by first splitting the input range according to the first map and then dealing with each of the output ranges separately. For the intersection we can use `applyMapClean` and for the others we call `applyMap` again, but this time with first map removed from the transform.

```hs
applyMap :: Range -> [Map] -> [Range]
applyMap inp [] = [inp]
applyMap inp (m@Map{to=a, from=b, len=c}:ms) = concatMap (tryApply m) intersections
      where tryApply map dat  | isIn (start dat) (from m, len m)  = [applyClean dat map]
                              | otherwise = applyMap dat ms
            isIn s (b, c) = s >= b && s < b+c
            intersections = splits inp (Range b (b+c))
```

This is the meat of solution implemented. All that remains is writing a function `applyMaps` to apply multiple transforms to a set of input ranges and then writing `solution2`.

```hs
applyMaps :: [Range] -> [[Map]] -> [Range]
applyMaps dat [] = dat
applyMaps dat@(d:ds) maps@(m:ms) = applyMaps (concatMap (`applyMap` m) dat) ms

solution2 :: [Range] -> [[Map]] -> Int
solution2 seeds maps = start. minimumBy (\a b -> compare (start a) (start b)) $ applyMaps seeds maps
```

This ended up being so drastically different from my solution for part1 that I had to write it in a separate file [Day05\_part2.hs](Day05_part2.hs). However, method can also solve part 1 by consider each seed as a range of length 1. I quickly wrote a `getSeedRanges1` for this:

```hs
getSeedRanges1 :: [Int] -> [Range]
getSeedRanges1 [] = []
getSeedRanges1 (x:res)  = Range x (x+1) : getSeedRanges1 res
```

This can be written in a single line with a map, but I wanted to keep it similar to the `getSeedRanges` of part2.
