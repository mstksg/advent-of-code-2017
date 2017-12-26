module AOC2017.Day13 (day13a, day13b) where

import           AOC2017.Types (Challenge)
import           Data.Char     (isDigit)
import           Data.Foldable (find)
import           Data.Maybe    (fromJust)


caughtAt
    :: Int          -- delay
    -> (Int, Int)   -- depth, range
    -> Bool
caughtAt delay (d, r) = triangle (r - 1) (d + delay) == 0
  where
    triangle n x = abs ((x - n) `mod` (n * 2) - n)

parse :: String -> [(Int, Int)]
parse = map parseLine . lines
  where
    parseLine (words->x:n:_) = (read (filter isDigit x), read n)
    parseLine _              = error "No parse"

day13a :: Challenge
day13a = show . sum . map (uncurry (*))
       . filter (caughtAt 0)
       . parse

day13b :: Challenge
day13b (parse->xs) = show . fromJust $ find neverCaughtWith [0..]
  where
    neverCaughtWith delay = all (not . caughtAt delay) xs
