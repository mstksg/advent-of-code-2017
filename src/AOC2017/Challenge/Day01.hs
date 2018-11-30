module AOC2017.Challenge.Day01 (day01a, day01b) where

import           AOC2017.Types (Challenge)
import           Data.Char     (isDigit, digitToInt)

conseqs :: [a] -> [(a,a)]
conseqs []     = []
conseqs (x:xs) = zip (x:xs) (xs ++ [x])

bisect :: [a] -> ([a], [a])
bisect xs = splitAt (length xs `div` 2) xs

matchings :: Eq a => [(a,a)] -> [a]
matchings = map fst . filter (uncurry (==))

parse :: String -> [Int]
parse = map digitToInt . filter isDigit

day01a :: Challenge
day01a = show . sum . matchings . conseqs . parse

day01b :: Challenge
day01b = show . (*2) . sum . matchings . uncurry zip . bisect . parse
