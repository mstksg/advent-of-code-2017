module AOC2017.Challenge.Day05 (day05a, day05b) where

import           AOC2017.Types     (Challenge)
import           AOC2017.Util      (iterateMaybe)
import           AOC2017.Util.Tape (Tape(..), move, unsafeTape)

-- | Update the focused cell and follow the requested jump, if possible
step
    :: (Int -> Int)         -- ^ cell update function
    -> Tape Int
    -> Maybe (Tape Int)
step f (Tape ls x rs) = move x (Tape ls (f x) rs)

parse :: String -> Tape Int
parse = unsafeTape . map read . lines

day05a :: Challenge
day05a = show . length . iterateMaybe (step succ  ) . parse

day05b :: Challenge
day05b = show . length . iterateMaybe (step update) . parse
  where
    update x
      | x >= 3    = x - 1
      | otherwise = x + 1

