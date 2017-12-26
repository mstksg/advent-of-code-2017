module AOC2017.Day14 (day14a, day14b) where

import           AOC2017.Day10          (knothash)
import           AOC2017.Types          (Challenge)
import           AOC2017.Util           (strip)
import           AOC2017.Util.Disjoints (Disjoints(..), disjoint)
import           Data.Ix                (index, range)
import           Text.Printf            (printf)
import qualified Data.IntSet            as IS
import qualified Data.Set               as S

mkGrid :: String -> [[Bool]]
mkGrid (strip->k) = map mkRow [0..127]
  where
    mkRow :: Int -> [Bool]
    mkRow n = map (== '1') . concatMap (printf "%08b") . knothash
            $ k ++ "-" ++ show n

day14a :: Challenge
day14a = show . length . filter id . concat . mkGrid

day14b :: Challenge
day14b = show . S.size . getD . litGroups . mkGrid

litGroups :: [[Bool]] -> Disjoints
litGroups grid = foldMap go (range r)
  where
    r = ((0,0),(127,127))
    isLit (x,y) = grid !! y !! x
    go p | isLit p   = disjoint . IS.fromList
                     . map (index r) . (p:) . filter isLit
                     $ neighbors p
         | otherwise = mempty

neighbors :: (Int, Int) -> [(Int, Int)]
neighbors (x,y) = [ (x+dx, y+dy) | (dx, dy) <- [(0,1),(0,-1),(1,0),(-1,0)]
                                 , x+dx >= 0
                                 , y+dy >= 0
                                 , x+dx < 128
                                 , y+dy < 128
                  ]
