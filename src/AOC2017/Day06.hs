module AOC2017.Day06 (day06a, day06b) where

import           AOC2017.Types  (Challenge)
import           Data.Bifunctor
import           Data.Maybe
import           Data.Tuple
import qualified Data.IntMap    as IM
import qualified Data.Map       as M

dropBlocks
    :: Int                  -- ^ number to drop
    -> Int                  -- ^ index to start dropping at
    -> IM.IntMap Int
    -> IM.IntMap Int
dropBlocks 0 _ m0 = m0
dropBlocks n i m0 = dropBlocks (n - 1) i' . IM.adjust (+ 1) i' $ m0
  where
    i' = (i + 1) `mod` (fst (IM.findMax m0) + 1)

step :: IM.IntMap Int -> IM.IntMap Int
step m0 = dropBlocks maxBlocks maxIx $ IM.insert maxIx 0 m0
  where
    (maxBlocks, maxIx) = second negate
                       . maximum
                       . map (second negate . swap)
                       . IM.toList
                       $ m0

-- | Returns the location of the first loop, and the length of the loop
findLoop :: Ord a => [a] -> Maybe (Int, Int)
findLoop = go 0 M.empty
  where
    go _ _ []     = Nothing
    go n m (x:xs) = case M.lookup x m of
      Just l  -> Just (n, l)
      Nothing -> let m' = (+1) <$> m
                 in  go (n + 1) (M.insert x 1 m') xs

day06 :: String -> (Int, Int)
day06 = fromJust . findLoop . iterate step . IM.fromList
      . zip [0..] . map read . words

day06a :: Challenge
day06a = show . fst . day06

day06b :: Challenge
day06b = show . snd . day06
