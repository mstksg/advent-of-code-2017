module AOC2017.Day15 (day15a, day15b) where

import           AOC2017.Types (Challenge)
import           Data.Char

generate :: Int -> Int -> Int
generate fac = go
  where
    go = (`mod` 2147483647) . (* fac) 

facA, facB :: Int
facA = 16807
facB = 48271

day15a = undefined
-- day15a :: Challenge
-- day15a (map grab.lines->a:b:_) = show . length . filter p $ zip (iterate (generate facA) a)
--                                                                 (iterate (generate facB) b)
--   where
--     p (x,y) = x `mod` 

-- grab = read . filter isDigit

day15b :: Challenge
day15b = undefined

