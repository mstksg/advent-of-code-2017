module AOC2017.Day16 (day16a, day16b) where

import           AOC2017.Types   (Challenge)
import           Data.Char       (chr, ord)
import           Data.List.Split (splitOn)
import           Data.Semigroup  (Semigroup(..), stimes, Dual(..))
import qualified Data.Map        as M

newtype Perm a = P { permMap :: M.Map a a }
    deriving Show

lookupPerm :: Ord a => Perm a -> a -> a
lookupPerm p k = M.findWithDefault k k (permMap p)

instance Ord a => Semigroup (Perm a) where
    x <> y = P $ (lookupPerm x <$> permMap y) `M.union` permMap x
instance Ord a => Monoid (Perm a) where
    mappend = (<>)
    mempty  = P M.empty

type Dance = (Perm Int, Dual (Perm Char))

runDance :: Dance -> String
runDance (pI, pN) = lookupPerm (getDual pN)
                  . toName
                  . lookupPerm pI
                <$> [0..15]
  where
    toName c = chr (c + ord 'a')

parseMove :: String -> Dance
parseMove = \case
    's':(read->n)                     -> (rotator n  , mempty            )
    'x':(map read.splitOn "/"->n:m:_) -> (swapper n m, mempty            )
    'p':n:_:m:_                       -> (mempty     , Dual (swapper n m))
    _                                 -> error "No parse"
  where
    rotator :: Int -> Perm Int
    rotator n = P $ M.fromList [ (i, (i - n) `mod` 16) | i <- [0..15] ]
    swapper :: Ord a => a -> a -> Perm a
    swapper x y = P $ M.fromList [ (x, y), (y, x) ]

parse :: String -> Dance
parse = foldMap parseMove . splitOn ","

day16a :: Challenge
day16a = runDance . parse

day16b :: Challenge
day16b = runDance . stimes (1e9 :: Int) . parse
