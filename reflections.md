Reflections
===========

Day 1
-----

*([code][d1c])*

[d1c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Day01.hs

We can generate a list of consecutive items (while looping around) very crudely
using:

```haskell
conseqs :: [a] -> [(a,a)]
conseqs (x:xs) = zip (x:xs) (xs ++ [x])
```

For part 2, we can generate a list of "opposite" items by zipping a bisected
list:

```haskell
bisect :: [a] -> ([a], [a])
bisect xs = splitAt (length xs `div` 2) xs

uncurry zip . bisect :: [a] -> [(a,a)]
```

From either of these, we can select the ones that are "matching" by filtering
for equal tuples:

```haskell
matchings :: Eq a => [(a,a)] -> [a]
matchings = map fst . filter (\(x,y) -> x == y)
```

The result is the sum of all of the "matched" numbers, so in the end, we have:

```haskell
day01a :: [Int] -> Int
day01a =        sum . matchings . (      conseqs       )

day01b :: [Int] -> Int
day01b = (*2) . sum . matchings . (uncurry zip . bisect)
```

Note that we do need to "double count" for Part 2.

We could parse the actual strings into `[Int]` by just using
`map digitToInt :: String -> [Int]`

### Day 1 Benchmarks

```
>> Day 01a
benchmarking...
time                 59.08 μs   (56.52 μs .. 61.98 μs)
                     0.981 R²   (0.971 R² .. 0.991 R²)
mean                 61.41 μs   (57.81 μs .. 69.65 μs)
std dev              17.28 μs   (7.177 μs .. 28.41 μs)
variance introduced by outliers: 97% (severely inflated)

>> Day 01b
benchmarking...
time                 93.48 μs   (88.50 μs .. 98.63 μs)
                     0.979 R²   (0.969 R² .. 0.992 R²)
mean                 90.52 μs   (87.72 μs .. 94.51 μs)
std dev              10.30 μs   (6.708 μs .. 14.16 μs)
variance introduced by outliers: 86% (severely inflated)
```

Day 2
-----

*([code][d2c])*

[d2c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Day02.hs

Good stream processing demonstration.  Both problems just boil down to summing
a function on all lines:

```haskell
day02a :: [[Int]] -> Int
day02a = sum . map checkA

day02b :: [[Int]] -> Int
day02b = sum . map checkB
```

`checkA` is just the maximum minus the minimum:

```haskell
checkA :: [Int] -> Int
checkA xs = maximum xs - minimum xs
```

`checkB` requires you to "find" an item subject to several constraints, and
this can be done using the list monad instance (to pretend to be writing
Prolog) or simply a list comprehension.

```haskell
checkB :: [Int] -> Int
checkB xs = head $ do
    y:ys   <- tails (sort xs)
    (d, 0) <- (`divMod` y) <$> ys
    return d
```

First we list all of our "possibilities" that we want to search -- we consider
all `y : ys`, where `y` is some element in our list, and `ys` is all of items
greater than or equal to `y` in the list.

Then we consider the `divMod` of any number in `ys` by `y`, but only the ones
that give a `mod` of 0 (the *perfect divisor* of `y` in `ys`).

Our result is `d`, the result of the perfect division.

Parsing is pretty straightforward again; we can use `map (map read . words) .
lines :: String -> [[Int]]` to split by lines, then by words, and `read` every
word.

### Day 2 Benchmarks

```
>> Day 02a
benchmarking...
time                 701.8 μs   (671.5 μs .. 741.4 μs)
                     0.982 R²   (0.961 R² .. 0.996 R²)
mean                 687.1 μs   (670.0 μs .. 721.0 μs)
std dev              80.53 μs   (50.15 μs .. 132.3 μs)
variance introduced by outliers: 81% (severely inflated)

>> Day 02b
benchmarking...
time                 775.4 μs   (742.7 μs .. 822.8 μs)
                     0.974 R²   (0.947 R² .. 0.996 R²)
mean                 769.2 μs   (746.3 μs .. 818.0 μs)
std dev              107.1 μs   (49.91 μs .. 186.3 μs)
variance introduced by outliers: 85% (severely inflated)
```

Day 3
-----

*([code][d3c])*

[d3c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Day03.hs

My Day 3 solution revolves around the `Trail` monoid:

```haskell
newtype Trail a = Trail { runTrail :: a -> ([a], a) }
instance Semigroup (Trail a) where
    f <> g = Trail $ \x -> let (xs, y) = runTrail f x
                               (ys, z) = runTrail g y
                           in  (xs ++ ys, z)
instance Monoid (Trail a) where
    mempty  = Trail ([],)
    mappend = (<>)
```

Which describes a function that "leaves a trail" as it is being run.  The
`mappend`/`<>` action composes two functions together (one after the other),
and also combines the "trails" that they leave behind.

In an unrelated monoid usage, we have

```haskell
type Pos = (Sum Int, Sum Int)
```

So `p1 <> p2` will be the component-wise addition of two points.

To start off, we build `ulam :: [Pos]`, an *infinite list* of positions, starting
from the middle of the spiral and moving outwards.  `ulam !! 0` would be the
very center (the 1st position), `ulam !! 10` would be the 11th position, etc.

We build this spiral using `move`, our most basic `Trail`:

```haskell
move :: Pos -> Trail Pos
move p = Trail $ \p0 -> ([p0 <> p], p0 <> p)
```

`move (1,0)` would give a `Trail` that *moves* one tile to the right, and
leaves the new position in its trail.


We can then build the entire spiral by `<>`ing (using `foldMap`) `Trail`s
forever:

```haskell
spiral :: Trail Pos
spiral = move (0,0)
      <> foldMap loop [1..]
  where
    loop :: Int -> Trail Pos
    loop n = stimes (2*n-1) (move ( 1, 0))
          <> stimes (2*n-1) (move ( 0, 1))
          <> stimes (2*n  ) (move (-1, 0))
          <> stimes (2*n  ) (move ( 0,-1))
```

And for `ulam`, we run the `Trail` from `(0,0)`, and get the trail list (`fst`).

```haskell
ulam :: [Pos]
ulam = fst $ runTrail spiral (0,0)
```

### Part 1

Part 1 is then just getting the `nth` item in `ulam`, and calculating its
distance from the center:

```haskell
day03a :: Int -> Int
day03a i = norm $ ulam !! (i - 1)
  where
    norm (Sum x, Sum y) = abs x + abs y
```

### Part 2

For Part 2, we keep the state of the filled out cells as a `Map Pos Int`, which
stores the number at each position.  If the position has not been "reached"
yet, it will not be in the `Map`.

We can use `State` to compose these functions easily.  Here we write a function
that takes a position and fills in that position's value in the `Map`
appropriately, and returns the new value at that position:


```haskell
updateMap :: Pos -> State (M.Map Pos Int) Int
updateMap p = state $ \m0 ->
    let newPos = sum . mapMaybe (`M.lookup` m0) $
          [ p <> (Sum x, Sum y) | x <- [-1 .. 1]
                                , y <- [-1 .. 1]
                                , x /= 0 || y /= 0
                                ]
    in  (newPos, M.insertWith (const id) p newPos m0)
```

We use `M.insertWith (const id)` instead of `M.insert` because we don't want to
overwrite any previous entries.

Since we wrote `updateMap` using `State`, we can just `traverse` over `ulam` --
if `updateMap p` updates the map at point `p` and returns the new value at that
position, then `traverse updateMap ulam` updates updates the map at every
position in `ulam`, one-by-one, and returns the new values at each position.

```haskell
cellNums :: [Int]
cellNums = flip evalState (M.singleton (0, 0) 1) $
    traverse updateMap ulam
```

And so part 2 is just finding the first item matching some predicate, which is
just `find` from *base*:

```haskell
day03b :: Int -> Int
day03b i = fromJust $ find (> i) cellNums
```

### Day 3 Benchmarks

```
>> Day 03a
benchmarking...
time                 2.706 ms   (2.640 ms .. 2.751 ms)
                     0.997 R²   (0.995 R² .. 0.999 R²)
mean                 2.186 ms   (2.090 ms .. 2.267 ms)
std dev              231.4 μs   (198.3 μs .. 267.7 μs)
variance introduced by outliers: 66% (severely inflated)

>> Day 03b
benchmarking...
time                 2.999 μs   (2.639 μs .. 3.438 μs)
                     0.870 R²   (0.831 R² .. 0.935 R²)
mean                 3.684 μs   (2.945 μs .. 4.457 μs)
std dev              1.629 μs   (1.190 μs .. 2.117 μs)
variance introduced by outliers: 99% (severely inflated)
```

Day 4
-----

*([code][d4c])*

[d4c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Day04.hs

Day 4 is very basic stream processing.  Just filter for lines that have "all
unique" items, and count how many lines are remaining.

Part 1 and Part 2 are basically the same, except Part 2 checks for uniqueness
up to ordering of letters.  If we sort the letters in each word first, this
normalizes all of the words so we can just use `==`.

```haskell
day04a :: [[String]] -> Int
day04a = length . filter uniq

day04b :: [[String]] -> Int
day04b = length . filter uniq . (map . map) sort
```

All that's left is finding a function to tell us if all of the items in a list
are unique.

```haskell
uniq :: Eq a => [a] -> Bool
uniq xs = length xs == length (nub xs)
```

There are definitely ways of doing this that scale better, but given that all
of the lines in my puzzle input are less than a dozen words long, it's really
not worth it to optimize!

(We can parse the input into a list of list of strings using
`map words . lines :: String -> [[String]]`)

### Day 4 Benchmarks
```
>> Day 04a
benchmarking...
time                 1.786 ms   (1.726 ms .. 1.858 ms)
                     0.990 R²   (0.984 R² .. 0.995 R²)
mean                 1.776 ms   (1.738 ms .. 1.877 ms)
std dev              193.2 μs   (98.00 μs .. 356.9 μs)
variance introduced by outliers: 73% (severely inflated)

>> Day 04b
benchmarking...
time                 3.979 ms   (3.431 ms .. 4.421 ms)
                     0.912 R²   (0.852 R² .. 0.974 R²)
mean                 3.499 ms   (3.349 ms .. 3.805 ms)
std dev              703.5 μs   (475.7 μs .. 1.026 ms)
variance introduced by outliers: 88% (severely inflated)
```

Day 5
-----

*([code][d5c])*

[d5c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Day05.hs

Day 5 centers around the `Tape` zipper:

```haskell
data Tape a = Tape { _tLefts  :: [a]
                   , _tFocus  :: a
                   , _tRights :: [a]
                   }
  deriving Show
```

We have the "focus" (the current pointer position), the items to the left of
the focus (in reverse order, starting from the item closest to the focus), and
the items to the right of the focus.

Tape is neat because moving one step to the left or right is O(1).  It's also
"type-safe" in our situation, unlike an `IntMap`, because it enforces a solid
unbroken tape space.

One fundamental operation on a tape is `move`, which moves the focus on a tape
to the left or right by an `Int` offset.  If we ever reach the end of the list,
it's `Nothing`.

```haskell
-- | `move n` is O(n)
move :: Int -> Tape Int -> Maybe (Tape Int)
move n (Tape ls x rs) = case compare n 0 of
    LT -> case ls of
      []    -> Nothing
      l:ls' -> move (n + 1) (Tape ls' l (x:rs))
    EQ -> Just (Tape ls x rs)
    GT -> case rs of
      []    -> Nothing
      r:rs' -> move (n - 1) (Tape (x:ls) r rs')
```

Now we just need to simulate the machine in the puzzle:

```haskell
step
    :: (Int -> Int)         -- ^ cell update function
    -> Tape Int
    -> Maybe (Tape Int)
step f (Tape ls x rs) = move x (Tape ls (f x) rs)
```

At every step, move based on the item at the list focus, and update that item
accordingly.

We can write a quick utility function to continually apply a `a -> Maybe a`
until we hit a `Nothing`:

```haskell
iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x0 = x0 : unfoldr (fmap dup . f) x0
  where
    dup x = (x,x)
```

And now we have our solutions.  Part 1 and Part 2 are pretty much the same,
except for different updating functions.

```haskell
day05a :: Tape Int -> Int
day05a = length . iterateMaybe (step update)
  where
    update x = x + 1

day05b :: Tape Int -> Int
day05b = length . iterateMaybe (step update)
  where
    update x
      | x >= 3    = x - 1
      | otherwise = x + 1
```

Note that we do have to parse our `Tape` from an input string.  We can do this
using something like:

```haskell
parse :: String -> Tape Int
parse (map read.lines->x:xs) = Tape [] x xs
parse _                      = error "Expected at least one line"
```

Parsing the words in the line, and setting up a `Tape` focused on the far left
item.

### Day 5 Benchmarks

```
>> Day 05a
benchmarking...
time                 514.3 ms   (417.9 ms .. 608.1 ms)
                     0.995 R²   (0.983 R² .. 1.000 R²)
mean                 479.1 ms   (451.4 ms .. 496.5 ms)
std dev              26.27 ms   (0.0 s .. 30.17 ms)
variance introduced by outliers: 19% (moderately inflated)

>> Day 05b
benchmarking...
time                 1.196 s    (1.164 s .. 1.265 s)
                     1.000 R²   (0.999 R² .. 1.000 R²)
mean                 1.211 s    (1.197 s .. 1.221 s)
std dev              15.45 ms   (0.0 s .. 17.62 ms)
variance introduced by outliers: 19% (moderately inflated)
```

Day 6
-----

*([code][d6c])*

[d6c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Day06.hs

Day 6 is yet another simulation of a virtual machine.  There might be an
analytic way to do things, but input size is small enough that you can just
directly simulate the machine in a reasonable time.

### Step

At the most basic level, we need to write a function to advance the simulation
one step in time:

```haskell
step :: V.Vector Int -> V.Vector Int
step v = V.accum (+) v' ((,1) <$> indices)
  where
    maxIx     = V.maxIndex v
    numBlocks = v V.! maxIx
    v'        = v V.// [(maxIx, 0)]
    indices   = (`mod` V.length v) <$> [maxIx + 1 .. maxIx + numBlocks]
```

`V.accum (+) v' ((,1) <$> indices)` will increment all indices in `indices` in
the vector by 1 -- potentially more than once times if it shows up in `indices`
multiple times.  For example, if `indices` is `[4,7,1,2,4]` will increment the
numbers at indices 4, 7, 1, 2, and 4 again (so the number at position 4 will be
incremented twice).

All that's left is generating `indices`.  We know we need an entry for every
place we want to "drop a block".  We get the starting index using `V.maxIndex`,
and so get the number of blocks to drop using `v V.! maxIx`.  Our list of
indices is just `[maxIx + 1 .. maxIx + numBlocks]`, but all `mod`'d by by the
size of `v` so we cycle through the indices.

We must remember to re-set the starting position's value to `0` before we
start.


### Loop

We can now just `iterate step :: [V.Vector Int]`, which just contains an
infinite list of steps.  We want to now find the loops.

To do this, we can scan across `iterate step`.  We keep track of a `m :: Map a
Int`, which stores all of the previously seen states (as keys), along with *how
long ago* they were seen. We also keep track of the number of steps we have
taken so far (`n`)

```haskell
findLoop :: Ord a => [a] -> (Int, Int)
findLoop = go 0 M.empty
  where
    go _ _ []     = error "We expect an infinite list"
    go n m (x:xs) = case M.lookup x m of
        Just l  -> (n, l)
        Nothing -> go (n + 1) (M.insert x 1 m') xs
      where
        m' = succ <$> m
```

At every step, if the `Map` *does* include the previously seen state as a key,
then we're done.  We return the associated value (how long ago it was seen) and
the number of steps we have taken so far.

Otherwise, insert the new state into the `Map`, update all of the old
"last-time-seen" values (by fmapping `succ`), and move on.

### All Together

We have our whole challenge:

```haskell
day06 :: V.Vector Int -> (Int, Int)
day06 = findLoop . iterate step
```

Part 1 is the `fst` of that, and Part 2 is the `snd` of that.

We can parse the input using `V.fromList . map read . words :: String ->
V.Vector Int`.

### Day 6 Benchmarks

```
>> Day 06a
benchmarking...
time                 735.3 ms   (696.4 ms .. NaN s)
                     0.999 R²   (0.998 R² .. 1.000 R²)
mean                 698.6 ms   (688.9 ms .. 707.4 ms)
std dev              14.59 ms   (0.0 s .. 15.25 ms)
variance introduced by outliers: 19% (moderately inflated)

>> Day 06b
benchmarking...
time                 622.8 ms   (556.6 ms .. 777.0 ms)
                     0.993 R²   (0.987 R² .. 1.000 R²)
mean                 685.0 ms   (658.1 ms .. 701.9 ms)
std dev              25.50 ms   (0.0 s .. 29.30 ms)
variance introduced by outliers: 19% (moderately inflated)
```

Day 7
-----

*([code][d7c])*

[d7c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Day07.hs

We can just build the tree in Haskell.  We have basically a simple rose tree of
`Int`s, so we can use `Tree Int` from `Data.Tree` (from the *containers*
package).

### Part 1

Our input is essentially `M.Map String (Int, S.Set String)`, a map of string
labels to their weights and the labels of their leaves.

```haskell
buildTree
    :: M.Map String (Int, S.Set String)
    -> (String, Tree Int)
buildTree m = (root, go root)
  where
    go :: String -> Tree Int
    go p = Node w (go <$> S.toList cs)
      where
        (w, cs) = m M.! p

    allChildren :: S.Set String
    allChildren = S.unions (snd <$> toList m)
    root :: String
    root = S.findMax $ M.keysSet m `S.difference` allChildren
```

Building a tree is pretty simple recursively -- just recursively look up the
children of our parent nodes.  The only complication is finding the "root" of
the entire tree.  This is simply the only symbol that is not in the union of
all children sets.

We technically don't need the strings in the tree, but we do need it for Part
1, so we can return it as a second input using a tuple.

```haskell
day07a :: M.Map String (Int, S.Set String) -> String
day07a = fst . buildTree
```

### Part 2

Time to find the bad node.

```haskell
findBad :: Tree Int -> Maybe Int
findBad t0 = listToMaybe badChildren <|> anomaly
  where
    badChildren :: [Int]
    badChildren = mapMaybe findBad $ subForest t0
    weightMap :: M.Map Int [Int]
    weightMap = M.fromListWith (++)
              . map (\t -> (totalWeight t, [rootLabel t]))
              . toList
              $ subForest t0
    anomaly :: Maybe Int
    anomaly = case sortOn (length . snd) (M.toList weightMap) of
      -- end of the line
      []                       -> Nothing
      -- all weights match
      [_]                      -> Nothing
      -- exactly one anomaly
      [(wTot1, [w]),(wTot2,_)] -> Just (w + (wTot2 - wTot1))
      -- should not happen
      _                        -> error "More than one anomaly for node"
    totalWeight :: Tree Int -> Int
    totalWeight = foldTree $ \x xs -> x + sum xs
```

At the heart of it all, we check if *any of the children* are bad, before
checking if the current node itself is bad.  This is because any anomaly on the
level of our current node is not fixable if there are any errors in children
nodes.

To isolate bad nodes, I built a `Map Int [Int]`, which is a map of unique
"total weight" to a list of all of the immediate child weights that have that
total weight.

If this map is empty, it means that there are no children.  `Nothing`, no
anomaly.

If this map has one item, it means that there is only one unique total weight
amongst all of the child nodes.  `Nothing`, no anomaly.

If the map has two items, it means that there are two distinct total weights,
and one of those should have exactly *one* corresponding child node.  (We can
sort the list to ensure that that anomaly node is the first one in the list)

From here we can compute what that anomaly node's weight (`w1`) should *really*
be, and return `Just` that.

Any other cases don't make sense (more than two distinct total weights, or a
situation where there isn't exactly one odd node)

```haskell
day07b :: M.Map String (Int, S.Set String) -> Int
day07b = findJust . findBad . snd . buildTree
```

### Parsing

Parsing is straightforward but not trivial.

```haskell
parseLine :: String -> (String, (Int, S.Set String))
parseLine (words->p:w:ws) =
    (p, (read w, S.fromList (filter isAlpha <$> drop 1 ws)))
parseLine _ = error "No parse"

parse :: String -> M.Map String (Int, S.Set String)
parse = M.fromList . map parseLine . lines
```

### Day 7 Benchmarks

```
>> Day 07a
benchmarking...
time                 8.562 ms   (7.067 ms .. 9.751 ms)
                     0.858 R²   (0.654 R² .. 0.961 R²)
mean                 9.634 ms   (8.727 ms .. 11.72 ms)
std dev              3.496 ms   (1.340 ms .. 5.820 ms)
variance introduced by outliers: 96% (severely inflated)

>> Day 07b
benchmarking...
time                 12.65 ms   (11.70 ms .. 13.86 ms)
                     0.953 R²   (0.899 R² .. 0.991 R²)
mean                 12.44 ms   (12.03 ms .. 13.30 ms)
std dev              1.409 ms   (1.024 ms .. 2.120 ms)
variance introduced by outliers: 58% (severely inflated)
```
