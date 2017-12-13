module AOC2017.Day12 (day12a, day12b) where

import           AOC2017.Types (Challenge)
import           Data.Char     (isDigit)
import           Data.List     (foldl', find)
import           Data.Maybe    (fromJust)
import qualified Data.IntSet   as IS
import qualified Data.Set      as S

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

parseLine :: String -> IS.IntSet
parseLine (words->n:_:ns) = IS.fromList $ read n
                                        : map (read . filter isDigit) ns
parseLine _               = error "No parse"

build :: String -> Disjoints
build = foldMap (D . S.singleton . parseLine) . lines

day12a :: Challenge
day12a = show . IS.size . fromJust . find (0 `IS.member`)
       . getD . build

day12b :: Challenge
day12b = show . S.size
       . getD . build
