
{-# LANGUAGE ViewPatterns #-}

module AOC2017.Day03 (day03a, day03b) where

import           AOC2017.Types
import           Control.Monad
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.List
import           Data.Maybe
import           Data.Semigroup
import qualified Data.Map                   as M

-- Why aren't these defined in base by default?
data EndoKleisli m a = EK { runEK :: a -> m a }
instance Monad m => Semigroup (EndoKleisli m a) where
    f <> g = EK (runEK f >=> runEK g)
instance Monad m => Monoid (EndoKleisli m a) where
    mempty  = EK return
    mappend = (<>)

type Trail a = EndoKleisli (Writer [a]) a

data Point = P !Int !Int
    deriving (Show, Eq, Ord)

norm :: Point -> Int
norm (P x y) = abs x + abs y

add :: Point -> Point -> Point
add (P x1 y1) (P x2 y2) = P (x1 + x2) (y1 + y2)

move :: Point -> Trail Point
move p = EK $ \p0 -> writer (add p0 p, [add p0 p])

spiral :: Trail Point
spiral = move (P 0 0)
      <> move (P 1 0)
      <> move (P 0 1)
      <> foldMap loop [1..]
  where
    loop :: Int -> Trail Point
    loop n = stimes (2*n  ) (move (P (-1)   0 ))
          <> stimes (2*n  ) (move (P   0  (-1)))
          <> stimes (2*n+1) (move (P   1    0 ))
          <> stimes (2*n+1) (move (P   0    1 ))

ulam :: [Point]
ulam = execWriter $ runEK spiral (P 0 0)

day03a :: Challenge
day03a (read->i) = show . norm $ ulam !! (i - 1)

updateMap :: Point -> State (M.Map Point Int) Int
updateMap p = state $ \m0 ->
    let Sum newPoint = foldMap (Sum . flip (M.findWithDefault 0) m0)
          [ p `add` P x y | x <- [-1 .. 1]
                          , y <- [-1 .. 1]
                          , x /= 0 || y /= 0
                          ]
    in  (newPoint, M.insert p newPoint m0)

cellNums :: State (M.Map Point Int) [Int]
cellNums = do
    put $ M.singleton (P 0 0) 1
    (1 :) <$> traverse updateMap (tail ulam)

day03b :: Challenge
day03b (read->i) = show . fromJust . find (> i) $ evalState cellNums M.empty

