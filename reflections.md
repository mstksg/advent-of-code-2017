Reflections
===========

Day 1
-----

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

Day 2
-----

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

Day 3
-----

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
day03a :: Challenge
day03a (read->i) = show . norm $ ulam !! (i - 1)
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
day03b :: Challenge
day03b (read->i) = show . fromJust $ find (> i) cellNums
```
