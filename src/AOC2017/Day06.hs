{-# LANGUAGE TupleSections #-}

module AOC2017.Day06 (day06a, day06b) where

import           AOC2017.Types (Challenge)
import           Control.Lens  (ix, (.~), (+~))
import           Data.Function ((&))
import           Data.Maybe
import qualified Data.Map      as M
import qualified Data.Vector   as V

dropBlocks
    :: Int                  -- ^ number to drop
    -> Int                  -- ^ index to start dropping at
    -> V.Vector Int
    -> V.Vector Int
dropBlocks n i0 v0 = snd $ iterate dropBlock (i0, v0) !! n
  where
    dropBlock (i, v) = (i', v & ix i' +~ 1)
      where
        i' = (i + 1) `mod` V.length v

step :: V.Vector Int -> V.Vector Int
step v = dropBlocks maxBlocks maxIx v'
  where
    maxIx     = V.maxIndex v
    maxBlocks = v V.! maxIx
    v'        = v & ix maxIx .~ 0

-- | Returns the location of the first loop, and the length of the loop
findLoop :: Ord a => [a] -> Maybe (Int, Int)
findLoop = go 0 M.empty
  where
    go _ _ []     = Nothing
    go n m (x:xs) = case M.lookup x m of
        Just l  -> Just (n, l)
        Nothing -> go (n + 1) (M.insert x 1 m') xs
      where
        m' = succ <$> m

day06 :: String -> (Int, Int)
day06 = fromJust . findLoop . iterate step
      . V.fromList . map read . words

day06a :: Challenge
day06a = show . fst . day06

day06b :: Challenge
day06b = show . snd . day06
