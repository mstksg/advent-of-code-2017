module AOC2017.Challenge.Day15 (day15a, day15b) where

import           AOC2017.Types (Challenge)
import           Data.Char     (isDigit)
import           Data.Function (on)
import           Data.Word     (Word16)

generate :: Int -> Int -> Int
generate fac = go
  where
    go = (`mod` 2147483647) . (* fac)

facA, facB :: Int
facA = 16807
facB = 48271

parse :: String -> (Int, Int)
parse inp = (a, b)
  where
    a:b:_ = read . filter isDigit <$> lines inp

day15a :: Challenge
day15a (parse->(seedA, seedB)) =
      show
    . countFirst 4e7
    $ zip (iterate (generate facA) seedA)
          (iterate (generate facB) seedB)

day15b :: Challenge
day15b (parse->(seedA, seedB)) =
      show
    . countFirst 5e6
    $ zip (filter (`divBy` 4) . iterate (generate facA) $ seedA)
          (filter (`divBy` 8) . iterate (generate facB) $ seedB)
  where
    x `divBy` b = x `mod` b == 0

countFirst
    :: Int
    -> [(Int, Int)]
    -> Int
countFirst n = length
             . filter (uncurry p)
             . take n
  where
    p = (==) @Word16 `on` fromIntegral
