Reflections
===========

[Table of Contents][]

[Table of Contents]: https://github.com/mstksg/advent-of-code-2017#reflections-and-benchmarks

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

Thanks to [glguy][] for the idea to use `accum`!

[glguy]: https://twitter.com/glguy

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
time                 681.9 ms   (658.3 ms .. 693.4 ms)
                     1.000 R²   (1.000 R² .. 1.000 R²)
mean                 669.9 ms   (665.6 ms .. 672.8 ms)
std dev              4.220 ms   (0.0 s .. 4.869 ms)
variance introduced by outliers: 19% (moderately inflated)

>> Day 06b
benchmarking...
time                 688.7 ms   (504.2 ms .. 881.2 ms)
                     0.990 R²   (0.964 R² .. 1.000 R²)
mean                 710.2 ms   (687.9 ms .. 731.7 ms)
std dev              36.52 ms   (0.0 s .. 37.19 ms)
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
-- | Returns the root label and the tree
buildTree
    :: M.Map String (Int, S.Set String)
    -> (String, Tree Int)
buildTree m = (root, result)
  where
    allChildren :: S.Set String
    allChildren = S.unions (snd <$> toList m)
    root :: String
    root = S.findMax $ M.keysSet m `S.difference` allChildren

    result :: Tree Int
    result = flip unfoldTree root $ \p ->
      let (w, cs) = m M.! p
      in  (w, toList cs)

```

Building a tree is pretty simple with `unfoldTree :: (a -> [b] -> (a,[b])) -> b
-> Tree a`.  Given an initial seed value, and a way to give a "result" (node
content) and all new seeds, it can unfold out a tree for us.  The initial seed
is the root node, and the unfolding process looks up the weights and all of the
children of the given label.

The only complication now is finding the "root" of the entire tree.  This is
simply the only symbol that is not in the union of all children sets.

We technically don't need the strings in the tree, but we do need it for Part
1, so we can return it as a second input using a tuple.

```haskell
day07a :: M.Map String (Int, S.Set String) -> String
day07a = fst . buildTree
```

One nice thing about using a tree is that we can actually visualize it using
`drawTree :: Tree String -> String` from *containers*!  It's kind of big though
so it's difficult to inspect for our actual input, but it's nice for being able
to check the sample input.

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
              . map (\t -> (sum t, [rootLabel t]))
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
```

At the heart of it all, we check if *any of the children* are bad, before
checking if the current node itself is bad.  This is because any anomaly on the
level of our current node is not fixable if there are any errors in children
nodes.

To isolate bad nodes, I built a `Map Int [Int]`, which is a map of unique
"total weight" to a list of all of the immediate child weights that have that
total weight.  We can build a total weight by just using `sum :: Tree Int ->
Int`, which adds up all of the weights of all of the child nodes.

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
day07b = fromJust . findBad . snd . buildTree
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
time                 8.411 ms   (7.956 ms .. 8.961 ms)
                     0.973 R²   (0.954 R² .. 0.989 R²)
mean                 8.129 ms   (7.939 ms .. 8.447 ms)
std dev              736.6 μs   (501.9 μs .. 1.058 ms)
variance introduced by outliers: 50% (moderately inflated)

>> Day 07b
benchmarking...
time                 12.30 ms   (11.36 ms .. 14.21 ms)
                     0.909 R²   (0.815 R² .. 0.993 R²)
mean                 12.06 ms   (11.55 ms .. 13.00 ms)
std dev              1.799 ms   (935.1 μs .. 2.877 ms)
variance introduced by outliers: 69% (severely inflated)
```

Day 8
-----

*([code][d8c])*

[d8c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Day08.hs

Happy to see that Day 8, like day 7, is another problem that is very suitable
for Haskell! :)

I decided to make an ADT to encode each instruction

```haskell
data Instr = Instr { _iRegister  :: String
                   , _iUpdate    :: Int
                   , _iCondReg   :: String
                   , _iPredicate :: Int -> Bool
                   }
```

It includes a register to update, an update amount, a register to check for a
condition, and a predicate to apply to see whether or not to apply an update.

So something like

```
b inc 5 if a > 1
```

would be parsed as

```haskell
Instr { _iRegister  = "b"
      , _iUpdate    = 5
      , _iCondReg   = "a"
      , _iPredicate = (> 1)
      }
```

From this, our updating function `step` is basically following the logic of the
puzzle's update process:

```haskell
step :: M.Map String Int -> Instr -> M.Map String Int
step m (Instr r u c p)
  | p (M.findWithDefault 0 c m) = M.insertWith (+) r u m
  | otherwise                   = m
```

### Part 1

So this makes Part 1 basically a simple `foldl`, to produce the final `Map` of
all the registers.  Then we use `maximum :: Ord v => Map k v -> v` to get the
maximum register value.

```haskell
day08a :: [Instr] -> Int
day08a = maximum . foldl' step M.empty
```

Note that this might potentially give the wrong answer if all register values
in the `Map` are negative.  Then `maximum` of our `Map` would be negative, but
there are still registers that exist with `0` that aren't in our `Map`.

### Part 2

Part 2 is basically a simple `scanl`.

```haskell
day08b :: [Instr] -> Int
day08b = maximum . foldMap toList . scanl' step M.empty
```

`foldl` gave us the *final* `Map`, but `scanl` gives us *all the intermediate*
`Map`s that were formed along the way.

We want the maximum value that was ever seen, so we use `foldMap toList :: [Map
k v] -> [v]` to get a list of all values ever seen, and `maximum` that list.
There are definitely more efficient ways to do this!  The same caveat
(situation where all registers are always negative) applies here.

By the way, isn't it neat that switching between Part 1 and Part 2 is just
switching between `foldl` and `scanl`?  (Observation thanks to [cocreature][])
Higher order functions and purity are the best!

[cocreature]: https://twitter.com/cocreature

### Parsing

Again, parsing an `Instr` is straightforward but non-trivial.

```haskell
parseLine :: String -> Instr
parseLine (words->r:f:u:_:c:o:x:_) =
    Instr { _iRegister  = r
          , _iUpdate    = f' (read u)
          , _iCondReg   = c
          , _iPredicate = (`op` read x)
          }
  where
    f' = case f of
      "dec" -> negate
      _     -> id
    op = case o of
      ">"  -> (>)
      ">=" -> (>=)
      "<"  -> (<)
      "<=" -> (<=)
      "==" -> (==)
      "!=" -> (/=)
      _    -> error "Invalid op"
parseLine _ = error "No parse"
```

Care has to be taken to ensure that `dec 5`, for instance, is parsed as an
update of `-5`.

It is interesting to note that -- as a consequence of laziness -- `read u` and
`f'` might never be evaluated, and `u` and `f` might never be parsed.  This is
because if the condition is found to be negative for a line, the `_iUpdate`
field is never used, so we can throw away `u` and `f` without ever evaluating
them!

### Day 8 Benchmarks

```
>> Day 08a
benchmarking...
time                 8.545 ms   (8.085 ms .. 9.007 ms)
                     0.984 R²   (0.975 R² .. 0.994 R²)
mean                 8.609 ms   (8.365 ms .. 9.328 ms)
std dev              1.068 ms   (432.2 μs .. 2.039 ms)
variance introduced by outliers: 67% (severely inflated)

>> Day 08b
benchmarking...
time                 9.764 ms   (9.185 ms .. 10.44 ms)
                     0.975 R²   (0.955 R² .. 0.993 R²)
mean                 9.496 ms   (9.233 ms .. 9.816 ms)
std dev              846.1 μs   (567.6 μs .. 1.200 ms)
variance introduced by outliers: 50% (moderately inflated)
```

Day 9
-----

*([code][d9c])*

[d9c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Day09.hs

Today I actually decided to [live stream][d9s] my leader board attempt!
Admittedly I was in a new and noisy environment, so adding live streaming to
that only made my attempt a bit more complicated :)

[d9s]: https://www.twitch.tv/videos/207969022

Anyway, our solution today involves the AST `Tree`:

```haskell
data Tree = Garbage String
          | Group [Tree]
```

Getting the score is a simple recursive traversal:

```haskell
treeScore :: Tree -> Int
treeScore = go 1
  where
    go _ (Garbage _ ) = 0
    go n (Group   ts) = n + sum (go (n + 1) <$> ts)

```

Getting the total amount of garbage is, as well:

```haskell
treeGarbage :: Tree -> Int
treeGarbage (Garbage n ) = length n
treeGarbage (Group   ts) = sum (treeGarbage <$> ts)
```

And so that's essentially our entire solution:

```haskell
day09a :: Tree -> Int
day09a = treeScore

day09b :: Tree -> Int
day09b = treeGarbage
```

### Parsing

Parsing was simpler than I originally thought it would be.  We can use the
*megaparsec* library's parser combinators:

```haskell
parseTree :: Parser Tree
parseTree = P.choice [ Group   <$> parseGroup
                     , Garbage <$> parseGarbage
                     ]
  where
    parseGroup :: Parser [Tree]
    parseGroup = P.between (P.char '{') (P.char '}') $
        parseTree `P.sepBy` P.char ','
    parseGarbage :: Parser String
    parseGarbage = P.between (P.char '<') (P.char '>') $
        catMaybes <$> many garbageChar
      where
        garbageChar :: Parser (Maybe Char)
        garbageChar = P.choice
          [ Nothing <$ (P.char '!' *> P.anyChar)
          , Just    <$> P.noneOf ">"
          ]
```

Our final `Tree` is either a `Group` (parsed with `parseGroup`) or `Garbage`
(parsed with `parseGarbage`).

*   `parseGroup` parses `Tree`s separated by `,`, between curly brackets.
*   `parseGarbage`  parses many consecutive valid garbage tokens (Which may or
    may not contain a valid garbage character, `Maybe Char`), between angled
    brackets.  It `catMaybe`s the contents of all of the tokens to get all
    actual garbage characters.

    Thanks to [rafl][] for the idea of using `many` and `between` for
    `parseGarbage` instead of my original explicitly recursive solution!

[rafl]: https://github.com/rafl

And so we have:

```haskell
parse :: String -> Tree
parse = either (error . show) id . P.runParser parseTree ""
```

We do need to handle the case where the parser doesn't succeed, since
`runParser` returns an `Either`.

### Day 9 Benchmarks

```
>> Day 09a
benchmarking...
time                 2.508 ms   (2.366 ms .. 2.687 ms)
                     0.957 R²   (0.910 R² .. 0.990 R²)
mean                 2.589 ms   (2.477 ms .. 3.009 ms)
std dev              628.2 μs   (223.5 μs .. 1.246 ms)
variance introduced by outliers: 94% (severely inflated)

>> Day 09b
benchmarking...
time                 3.354 ms   (3.108 ms .. 3.684 ms)
                     0.952 R²   (0.919 R² .. 0.992 R²)
mean                 3.196 ms   (3.086 ms .. 3.383 ms)
std dev              411.1 μs   (232.5 μs .. 595.1 μs)
variance introduced by outliers: 76% (severely inflated)
```

Day 10
------

*([code][d10c])* *([stream][d10s])*

[d10c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Day10.hs
[d10s]: https://www.twitch.tv/videos/208287550

I feel like I actually had a shot today, if it weren't for a couple of silly
mistakes! :(  First I forgot to add a number, then I had a stray newline in my
input for some reason.  For Day 9, I struggled to get an idea of what's going
on, but once I had a clear plan, the rest was easy.  For Day 10, the clear idea
was fast, but the many minor lapses along the way were what probably delayed me
the most :)

Our solution today revolves around this state type:

```haskell
data HashState = HS { _hsVec  :: V.Vector Int
                    , _hsPos  :: Word8
                    , _hsSkip :: Word8
                    }
```

Interesting note -- this `Vector, Int` pairing is actually something that has
come up *a lot* over the previous Advent of Code puzzles.  It's basically a
vector attached with some "index" (or "focus").  It's actually a manifestation
of the [*Store* Comonad][store].  Something like this really would have made a
lot of the previous puzzles really simple, or at least would have been very
suitable for their implementations.

[store]: http://hackage.haskell.org/package/comonad-5.0.2/docs/Control-Comonad-Store.html

### Part 1

Anyway, most of the algorithm boils down to a `foldl` with this state on some
list of inputs:

```haskell
step :: HashState -> Word8 -> HashState
step (HS v0 p0 s0) n = HS v1 p1 s1
  where
    ixes = fromIntegral . (+ p0) <$> init [0 .. n]
    vals = (v0 V.!) <$> ixes
    v1   = v0 V.// zip ixes (reverse vals)
    p1   = p0 + n + s0
    s1   = s0 + 1
```

Our updating function is somewhat of a direct translation of the requirements.
All of the indices to update are enumerated using `[0 .. n]`.  But, we only
want the first `n` items (we don't want to actually include `n`, just `n - 1`),
so we can take the `init` of it.  We shift their positions by `+ p0`.

The "trick" to the cyclic vector is that `Word8` addition is modular
arithmetic, so this will actually cause overflows to wrap around like we
require.  For example, `(+ 253) <$> [0..5]` is `[253,254,255,0,1,2]`

We also need the *values* at each of the indices, so we map `(v0 V.!)` over our
list of indices.

Finally, we use `(//) :: Vector a -> [(Int, a)] -> Vector a` to
update all of the items necessary.  `//` replaces all of the indices in the
list with the values they are paired up with.  For us, we want to put the items
back in the list in reverse order, so we zip `ixes` and `reverse vals`, so that
the indices at `ixes` are set to be the values `reverse vals`.

Our new position is `p0 + n + s0` -- the current position plus the length plus
the skip count.  Again, because of `Word8` arithmetic, this wraps around at
`255`, so it has the behavior we want.

Now we can iterate this using `foldl'`

```haskell
process :: [Word8] -> V.Vector Int
process = _hsVec . foldl' step hs0
  where
    hs0 = HS (V.generate 256 id) 0 0
```

From here, we can write our Part 1:

```haskell
day10a :: [Int] -> Int
day10a = product . V.take 2 . process
```

We can parse our input using `map read . splitOn "," :: String -> [Int]`,
`splitOn` from the *[split][]* library.

[split]: http://hackage.haskell.org/package/split

### Part 2

Part 2 is pretty straightforward in that the *logic* is extremely simple, just
do a series of transformations.


```haskell
day10b :: [Word8] -> String
day10b = toHex . process
       . concat . replicate 64 . (++ salt)
  where
    salt  = [17, 31, 73, 47, 23]
    toHex = concatMap (printf "%02x" . foldr xor 0) . chunksOf 16 . toList
    strip = T.unpack . T.strip . T.pack
```

We:

3.  Append the salt bytes at the end
4.  `concat . replicate 64 :: [a] -> [a]`, replicate the list of inputs 64 times
5.  `process` things like how we did in Part 1
7.  Convert to hex:
    *   Break into chunks of 16 (using `chunksOf` from the *[split][]* library)
    *   `foldr` each chunk of 16 using `xor`
    *   Convert the resulting `Int` to a hex string, using `printf %02x`
    *   Aggregate all of the chunk results later

Again, it's not super complicated, it's just that there are so many steps
described in the puzzle!

We can parse our input using `map (fromIntegral . ord) :: String -> [Word8]`,
taking care to also strip any leading and trailing whitespace first.

### Day 10 Benchmarks

*Note:* Benchmarks measured with *storable* vectors.

```
>> Day 10a
benchmarking...
time                 254.6 μs   (242.1 μs .. 268.9 μs)
                     0.925 R²   (0.851 R² .. 0.976 R²)
mean                 348.8 μs   (289.1 μs .. 467.6 μs)
std dev              265.2 μs   (147.4 μs .. 414.0 μs)
variance introduced by outliers: 99% (severely inflated)

>> Day 10b
benchmarking...
time                 25.00 ms   (21.24 ms .. 27.61 ms)
                     0.936 R²   (0.822 R² .. 0.992 R²)
mean                 26.21 ms   (24.47 ms .. 33.03 ms)
std dev              6.425 ms   (1.676 ms .. 13.03 ms)
variance introduced by outliers: 83% (severely inflated)
```

Day 11
------

*([code][d11c])*

[d11c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Day11.hs

Nothing too interesting here!  Just a straightforward application of the great
*[grid][]* library.

[grid]: https://hackage.haskell.org/package/grid


We barely need to wrap its `neighbor` function (which lets us move in a given
direction) for our usage:

```haskell
step :: (Int, Int) -> HexDirection -> (Int, Int)
step p = fromJust . neighbour UnboundedHexGrid p

day11a :: [HexDireciton] -> Int
day11a = distance UnboundedHexGrid (0,0) . foldl' step (0,0)
```

It's just a `foldl` of `neighbor`, and then finding the distance at the final
point.

And, like day 8's solution, all we need for Part 2 is to switch `foldl'` to
`scanl`:

```haskell
day11a :: [HexDireciton] -> Int
day11b = maximum . map (distance UnboundedHexGrid (0,0)) . scanl step (0,0)
```

`foldl` gives us the final position, but `scanl` gives us the intermediate
ones.  We just map our distance function onto all of the intermediate positions
to get a list of intermediate distances, and take the maximum of those.

The most time consuming part was probably writing the parsing function:

```haskell
parse :: String -> [HexDirection]
parse = map (parseDir . filter isAlpha) . splitOn ","
  where
    parseDir = \case
      "nw" -> Northwest
      "n"  -> North
      "ne" -> Northeast
      "se" -> Southeast
      "s"  -> South
      "sw" -> Southwest
      d    -> error $ "Bad direction " ++ d
```


Much thanks to [Amy de Buitléir][mhwombat] for the library, which does most of
the heavy lifting :)

[mhwombat]: https://github.com/mhwombat

### Day 11 Benchmarks

```
>> Day 11a
benchmarking...
time                 6.331 ms   (5.971 ms .. 6.778 ms)
                     0.960 R²   (0.917 R² .. 0.992 R²)
mean                 6.974 ms   (6.444 ms .. 8.575 ms)
std dev              2.855 ms   (528.3 μs .. 5.360 ms)
variance introduced by outliers: 97% (severely inflated)

>> Day 11b
benchmarking...
time                 7.267 ms   (7.017 ms .. 7.503 ms)
                     0.988 R²   (0.976 R² .. 0.995 R²)
mean                 7.337 ms   (7.172 ms .. 7.586 ms)
std dev              563.7 μs   (392.0 μs .. 794.2 μs)
variance introduced by outliers: 44% (moderately inflated)
```

Day 12
------

*([code][d12c])*

[d12c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Day12.hs

For Day 12, I made a monoid that is collection of disjoint sets, which we use
to model the set of distinct "groups" in our puzzle.  The sets represent things
that are all interconnected.

```haskell
newtype Disjoints = D { getD :: S.Set IS.IntSet }
instance Monoid Disjoints where
    mempty        = D S.empty
    mappend xs ys = foldl' go ys (getD xs)
      where
        go (D zs) z = D (newGroup `S.insert` disjoints)
          where
            overlaps  = S.filter (not . IS.null . (`IS.intersection` z)) zs
            disjoints = zs `S.difference` overlaps
            newGroup  = IS.unions $ z : S.toList overlaps

```

The mappend action is union, but preserving disjoint connection property.  If
we assume that all items in a set are connected, then the merger of two
collections of disjoint groups will be a new collection of disjoint groups,
merging together any of the original sets if it is found out that their items
have any connections.

For example, merging `DG [[3,5],[8,9],[10,11]]` with `DG [[5,6,8]]` will give
`DG [[3,5,6,8,9], [10,11]]`.

Now our entire thing is just a `foldMap`.  If we treat each of the original
lines as `IS.IntSet`, a set of connected things:


```haskell
build :: [IS.IntSet] -> Disjoints
build = foldMap (D . S.singleton)
```

where `D . S.singleton :: IS.IntSet -> Disjoints`, the "single group"
`Disjoints`.

From here, querying for the size of the group containing `0`, and the number of
groups total, is pretty simple:


```haskell
day12a :: [IS.IntSet] -> Int
day12a = IS.size . fromJust
       . find (0 `IS.member`)
       . getD . build

day12b :: [IS.IntSet] -> Int
day12b = S.size . getD . build
```

Part 2 is even simpler than Part 1!

Parsing is again straightforward:

```haskell
parseLine :: String -> IS.IntSet
parseLine (words->n:_:ns) = IS.fromList $ read n
                                        : map (read . filter isDigit) ns
parseLine _               = error "No parse"

parse :: String -> [IS.IntSet]
parse = map parseLine . lines
```

### Day 12 Benchmarks

```
>> Day 12a
benchmarking...
time                 53.76 ms   (44.69 ms .. 59.42 ms)
                     0.961 R²   (0.859 R² .. 0.999 R²)
mean                 58.32 ms   (54.25 ms .. 73.01 ms)
std dev              12.51 ms   (1.563 ms .. 21.58 ms)
variance introduced by outliers: 73% (severely inflated)

>> Day 12b
benchmarking...
time                 51.23 ms   (44.52 ms .. 55.72 ms)
                     0.973 R²   (0.925 R² .. 0.998 R²)
mean                 59.26 ms   (54.50 ms .. 76.39 ms)
std dev              15.13 ms   (3.328 ms .. 26.23 ms)
variance introduced by outliers: 82% (severely inflated)
```

Day 13
------

*([code][d13c])*

[d13c]: https://github.com/mstksg/advent-of-code-2017/blob/master/src/AOC2017/Day13.hs

Day 13 is a puzzle that reveals itself nicely after putting in a moment to
think of some analytic solutions.

The motion of the scanners follows a [Triangle Wave][].  I picked up the
equation on the wikipedia page:

[Triangle Wave]: https://en.wikipedia.org/wiki/Triangle_wave

```haskell
triangle range t = abs ((t - range) `mod` (range * 2) - range)
```

This is a triangle wave starting at zero, that goes from `0` to `range`.

It's probably not the cleanest solution, but it works ok as a direct
translation!  We also don't need the `abs`, since we only care about when
`triangle range t == 0`, but it doesn't hurt to leave it there for clarity.

Now we can write a function to see if you are caught at a given depth and
range:

```
caughtAt
    :: Int          -- delay
    -> (Int, Int)   -- depth, range
    -> Bool
caughtAt delay (d, r) = triangle (r - 1) (d + delay) == 0
```

Our `range` is actually one less than `triangle`'s expected range (we travel
from `0` to `r-1`).  And, `t` is `depth + delay`.  That is, if our initial
delay is 0, then `t = depth` -- it will take us `depth` picoseconds to get to
that given depth.  In general, it will take us `depth + delay` picoseconds to
get to a given depth -- a contribution from waiting to start, and a
contribution from the time it will take to actually reach that depth once we
start.

