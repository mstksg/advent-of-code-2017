
module AOC2017.Util.Disjoints (
    Disjoints(..)
  , disjoint
  ) where

import           Data.List
import qualified Data.IntSet as IS
import qualified Data.Set    as S

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

disjoint :: IS.IntSet -> Disjoints
disjoint = D . S.singleton
