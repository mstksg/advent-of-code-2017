module AOC2017.Day14 (day14a, day14b) where

import           AOC2017.Day10
import           AOC2017.Types    (Challenge)
import           AOC2017.Util
import           Control.Monad
import           Data.Char
import           Data.Ix
import           Data.List
import qualified Data.IntSet      as IS
import qualified Data.Set         as S

-- | Monoid representing a collection of disjoint "connected sets"
newtype Disjoints = D { getD :: S.Set IS.IntSet }
instance Monoid Disjoints where
    mempty        = D S.empty
    -- | mappend is much faster if the smaller set is second
    mappend xs ys = foldl' go ys (getD xs)
      where
        go (D zs) z = D (newGroup `S.insert` disjoints)
          where
            overlaps  = S.filter (not . IS.null . (`IS.intersection` z)) zs
            disjoints = zs `S.difference` overlaps
            newGroup  = IS.unions $ z : S.toList overlaps

gridUp :: String -> [[Bool]]
gridUp (strip->k) = map mkRow [0..127]
  where
    mkRow :: Int -> [Bool]
    mkRow n = concatMap hexToBits . day10b $ k ++ "-" ++ show n
    hexToBits :: Char -> [Bool]
    hexToBits = (binaries !!) . digitToInt
    binaries :: [[Bool]]
    binaries = replicateM 4 [False,True]

day14a :: Challenge
day14a = show . length . filter id . concat . gridUp

day14b :: Challenge
day14b (gridUp->grid) = show . S.size . getD $ foldMap go (range r)
  where
    r = ((0,0),(127,127))
    isGood (x,y) = grid !! y !! x
    go p
      | isGood p  = D . S.singleton . IS.fromList
                  . map (index r)
                  . (p:)
                  . filter isGood
                  $ neighbors p
      | otherwise = mempty

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x,y) = [ (x+dx, y+dy) | (dx, dy) <- [(0,1),(0,-1),(1,0),(-1,0)]
                                 , x+dx >= 0
                                 , y+dy >= 0
                                 , x+dx < 128
                                 , y+dy < 128
                  ]

