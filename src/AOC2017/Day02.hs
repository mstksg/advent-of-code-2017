{-# LANGUAGE TypeApplications #-}

module AOC2017.Day02 (day02a, day02b) where

import           AOC2017.Types (Challenge)
import           Data.List     (sort, tails)
import           Data.Maybe    (listToMaybe)

parse :: String -> [[Int]]
parse = map (map (read @Int) . words) . lines

day02a :: Challenge
day02a = show . sum . map check . parse
  where
    check xs = maximum xs - minimum xs

day02b :: Challenge
day02b = maybe "Bad lines" (show . sum) . traverse (check . sort) . parse
  where
    -- a bit of prolog never hurt anyone?
    check xs = listToMaybe $ do
      y:ys      <- tails xs
      candidate <- ys
      (d, 0)    <- return $ candidate `divMod` y
      return d
