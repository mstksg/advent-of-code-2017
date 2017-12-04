{-# LANGUAGE TupleSections #-}

module AOC2017.Day04 (day04a, day04b) where

import           AOC2017.Types (Challenge)
import           Data.List     (sort)
import qualified Data.Set      as S

uniq :: Ord a => [a] -> Bool
uniq xs = length xs == length (S.fromList xs)

day04a :: Challenge
day04a = show . length . filter (uniq .            words) . lines

day04b :: Challenge
day04b = show . length . filter (uniq . map sort . words) . lines
