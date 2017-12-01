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

bisect :: [a] -> ([a],[a])
bisect = bimap reverse toList . snd . foldl' go (False, ([], Seq.empty))
  where
    go (False, (ys, zs      )) x = (True , (ys  , zs |> x        ))
    go (True , (ys, z :<| zs)) x = (False, (z:ys, zs |> x        ))
    go (True , (ys, Empty   )) x = (False, (ys  , Seq.singleton x))

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
