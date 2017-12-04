module AOC2017.Day01 (day01a, day01b) where

import           AOC2017.Types
import           Data.Bifunctor
import           Data.Char
import           Data.Foldable
import           Data.Sequence  (Seq(..), (|>))
import qualified Data.Sequence  as Seq

conseqs :: [a] -> [(a,a)]
conseqs []     = []
conseqs (x:xs) = zip (x:xs) (xs ++ [x])

bisect :: [a] -> ([a], [a])
bisect xs = splitAt (length xs `div` 2) xs

matchings :: Eq a => [(a,a)] -> [a]
matchings = map fst . filter (uncurry (==))

sumMatchings :: (Num a, Eq a) => [(a,a)] -> a
sumMatchings = sum . matchings

parse :: String -> [Int]
parse = map digitToInt . filter isDigit

day01a :: Challenge
day01a = show . sumMatchings . conseqs . parse

day01b :: Challenge
day01b = show . (*2) . sumMatchings . uncurry zip . bisect . parse
