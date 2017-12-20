module AOC2017.Day17 (day17a, day17b) where

import           AOC2017.Types     (Challenge)
import           AOC2017.Util.Tape (Tape(..), moveC)
import           Data.List         (elemIndices, foldl')

unshift :: a -> Tape a -> Tape a
unshift y (Tape ls x rs) = Tape (x:ls) y rs

run :: Int -> Tape Int -> Int -> Tape Int
run n t0 x = unshift x . moveC n $ t0

day17a :: Challenge
day17a (read->n) = show r
  where
    Tape _ _ (r:_) = foldl' (run n) (Tape [] 0 []) [1 .. 2017]

day17b :: Challenge
day17b (read->n) = show . last
                 . elemIndices @Int 1
                 $ scanl jump 0 [1 .. 5e7]
  where
    jump i x = ((i + n) `mod` x) + 1
