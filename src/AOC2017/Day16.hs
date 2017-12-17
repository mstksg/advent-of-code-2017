module AOC2017.Day16 (day16a, day16b) where

import           AOC2017.Types   (Challenge)
import           Data.Char       (chr, ord)
import           Data.List.Split (splitOn)
import           Data.Semigroup  (Semigroup(..), stimes)
import qualified Data.Map        as M

data Dance = DP { _dPos  :: !(M.Map Int Int)
                , _dName :: !(M.Map Char Char)
                }
  deriving Show

instance Semigroup Dance where
    DP p1 n1 <> DP p2 n2 = DP (p1 `comp` p2) (n2 `comp` n1)
      where
        comp m1 m2 = (follow <$> m2) `M.union` m1
          where
            follow i = M.findWithDefault i i m1
instance Monoid Dance where
    mappend = (<>)
    mempty = DP M.empty M.empty


runDance :: Dance -> String
runDance d = lookupPerm (_dName d)
           . toName
           . lookupPerm (_dPos d)
         <$> [0..15]
  where
    lookupPerm m k = M.findWithDefault k k m
    toName c       = chr (c + ord 'a')

parseMove :: String -> Dance
parseMove = \case
    's':(read->n)                     -> DP (rotator n)   M.empty
    'x':(map read.splitOn "/"->n:m:_) -> DP (swapper n m) M.empty
    'p':n:_:m:_                       -> DP M.empty       (swapper n m)
    _                                 -> error "No parse"
  where
    rotator :: Int -> M.Map Int Int
    rotator n = M.fromList [ (i, (i - n) `mod` 16) | i <- [0..15] ]
    swapper :: Ord a => a -> a -> M.Map a a
    swapper x y = M.fromList [ (x, y), (y, x) ]

parse :: String -> Dance
parse = foldMap parseMove . splitOn ","

day16a :: Challenge
day16a = runDance . parse

day16b :: Challenge
day16b = runDance . stimes (1e9 :: Int) . parse
