module AOC2017.Day17 (day17a, day17b) where

import           AOC2017.Types     (Challenge)
import           AOC2017.Util      ((!!!))
import           AOC2017.Util.Tape (Tape(..), moveRightC)
import           Data.List         (elemIndices)

unshift :: a -> Tape a -> Tape a
unshift y (Tape ls x rs) = Tape (x:ls) y rs

run :: Int -> [Tape Int]
run n = go 1 (Tape [] 0 [])
  where
    go x t0 = t0 : go (x + 1) t1
      where
        t1 = unshift x $ iterate moveRightC t0 !!! n

day17a :: Challenge
day17a (read->n) = show r
  where
    Tape _ _ (r:_) = run n !!! 2017

day17b :: Challenge
day17b (read->n) = show . last
                 . elemIndices @Int 1
                 $ scanl jump 0 [1 .. 5e7]
  where
    jump i x = ((i + n) `mod` x) + 1
