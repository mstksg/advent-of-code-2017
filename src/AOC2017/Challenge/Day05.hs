module AOC2017.Challenge.Day05 (day05a, day05b) where

import           AOC2017.Types
import           AOC2017.Util          (iterateMaybe)
import qualified Data.List.PointedList as PL

-- | Update the focused cell and follow the requested jump, if possible
step
    :: (Int -> Int)         -- ^ cell update function
    -> PL.PointedList Int
    -> Maybe (PL.PointedList Int)
step f = uncurry PL.moveN . PL.focus (\x -> (x, f x))

parse :: String -> Maybe (PL.PointedList Int)
parse = PL.fromList . map read . lines

day05a :: Challenge
day05a = runC C
    { cParse = parse
    , cShow  = show
    , cSolve = Just . length . iterateMaybe (step succ  )
    }

day05b :: Challenge
day05b = runC C
    { cParse = parse
    , cShow  = show
    , cSolve = Just . length . iterateMaybe (step update)
    }
  where
    update x
      | x >= 3    = x - 1
      | otherwise = x + 1

