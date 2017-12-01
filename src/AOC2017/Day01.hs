module AOC2017.Day01 (day01a, day01b) where

import           AOC2017.Types
import           Data.Char
import qualified Data.Sequence as Seq
import           Data.Sequence (Seq(..), (|>))
import           Data.Foldable

conseqs :: [a] -> [(a,a)]
conseqs []     = []
conseqs (x:xs) = zip (x:xs) (xs ++ [x])

bisect :: [a] -> ([a],[a])
bisect = go False [] Seq.empty
  where
    go _     ys zs         []     = (reverse ys, toList zs)
    go False ys zs         (x:xs) = go True  ys     (zs |> x        ) xs
    go True  ys Empty      (x:xs) = go False ys     (Seq.singleton x) xs
    go True  ys (z :<| zs) (x:xs) = go False (z:ys) (zs |> x        ) xs

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
