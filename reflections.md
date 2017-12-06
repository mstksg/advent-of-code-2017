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

Now, we can turn a string into a list of numbers using `map digitToInt`, so our
final (unsafe) solutions are:


```haskell
day01a :: Challenge
day01a = show .        sum . matchings .               conseqs . map digitToInt

day01b :: Challenge
day01b = show . (*2) . sum . matchings . uncurry zip . bisect  . map digitToInt
```

Note that we do need to "double count" the Part 2 amount.
