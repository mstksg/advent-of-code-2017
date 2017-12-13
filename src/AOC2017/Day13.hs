module AOC2017.Day13 (day13a, day13b) where

import           AOC2017.Types     (Challenge)
import           Data.Char
import           Data.Foldable
import           Data.Maybe


scanner
    :: Int  -- depth
    -> Int  -- shift
    -> Int
scanner d = triangle (d - 1)
  where
    triangle n x = abs ((x - n) `mod` (n * 2) - n)

run :: Int
    -> [(Int,Int)]
    -> [(Int, Int)]
run delay = mapMaybe sneak
  where
    sneak (n, d)
      | scanner d (n + delay) == 0 = Just (n, d)
      | otherwise                  = Nothing

parse :: String -> [(Int, Int)]
parse = map parseLine . lines
  where
    parseLine (words->x:n:_) = (read (filter isDigit x), read n)
    parseLine _              = error "No parse"

day13a :: Challenge
day13a = show . sum . map (uncurry (*)) . run 0 . parse

day13b :: Challenge
day13b (parse->xs) = show . fromJust . flip find [0..] $ \d ->
      null (run d xs)
