module AOC2017.Challenge.Day04 (day04a, day04b) where

import           AOC2017.Types (Challenge)
import           Data.List     (sort, nub)

uniq :: Eq a => [a] -> Bool
uniq xs = length xs == length (nub xs)

day04a :: Challenge
day04a = show . length . filter (uniq .            words) . lines

day04b :: Challenge
day04b = show . length . filter (uniq . map sort . words) . lines
