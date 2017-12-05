
{-# LANGUAGE ViewPatterns #-}

module AOC2017.Day05 (day05a, day05b, move, Tape(..)) where

import           AOC2017.Types (Challenge)
import           Data.List     (unfoldr)

data Tape a = Tape { _tLefts  :: [a]
                   , _tFocus  :: a
                   , _tRights :: [a]
                   }
  deriving Show

-- | Shifts the Tape to the left or right by a given amount
move :: Int -> Tape Int -> Maybe (Tape Int)
move n (Tape ls x rs) = case compare n 0 of
    LT -> case ls of
      []    -> Nothing
      l:ls' -> move (n + 1) (Tape ls' l (x:rs))
    EQ -> Just (Tape ls x rs)
    GT -> case rs of
      []    -> Nothing
      r:rs' -> move (n - 1) (Tape (x:ls) r rs')

-- | Update the focused cell and follow the requested jump, if possible
step
    :: (Int -> Int)         -- ^ cell update function
    -> Tape Int
    -> Maybe (Tape Int)
step f (Tape ls x rs) = move x (Tape ls (f x) rs)

parse :: String -> Tape Int
parse (map read.lines->x:xs) = Tape [] x xs
parse _                      = error "Expected at least one line"

day05a :: Challenge
day05a = show . length . unfold_ (step succ  ) . parse

day05b :: Challenge
day05b = show . length . unfold_ (step update) . parse
  where
    update x
      | x >= 3    = x - 1
      | otherwise = x + 1

unfold_ :: (a -> Maybe a) -> a -> [a]
unfold_ f = unfoldr (fmap dup . f)
  where
    dup x = (x,x)

