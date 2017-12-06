module AOC2017.Day02 (day02a, day02b) where

import           AOC2017.Types (Challenge)
import           Data.List     (sort, tails)

parse :: String -> [[Int]]
parse = map (map read . words) . lines

day02a :: Challenge
day02a = show . sum . map check . parse
  where
    check xs = maximum xs - minimum xs

day02b :: Challenge
day02b = show . sum . map check . parse
  where
    -- a bit of prolog never hurt anyone?
    check xs = head $ do
      y:ys   <- tails (sort xs)
      (d, 0) <- (`divMod` y) <$> ys
      return d
