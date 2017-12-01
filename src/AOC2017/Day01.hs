
module AOC2017.Day01 (day01) where

import           AOC2017.Types
import           Data.Char

matchings :: Eq a => [a] -> [a]
matchings []     = []
matchings (x:xs) = map fst . filter (uncurry (==)) $ zip (x:xs) (xs ++ [x])

sumMatchings :: (Num a, Eq a) => [a] -> a
sumMatchings = sum . matchings

day01 :: Challenge
day01 = show . sumMatchings . map digitToInt . filter isDigit
