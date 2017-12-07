{-# LANGUAGE TupleSections #-}

module AOC2017.Day06 (day06a, day06b) where

import           AOC2017.Types    (Challenge)
import           Data.Maybe
import qualified Data.Map         as M
import qualified Data.Vector      as V

step :: V.Vector Int -> V.Vector Int
step v = V.accum (+) v' [ (i `mod` V.length v, 1)
                        | i <- [maxIx + 1 .. maxIx + maxBlocks]
                        ]
  where
    maxIx     = V.maxIndex v
    maxBlocks = v V.! maxIx
    v'        = v V.// [(maxIx, 0)]

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
