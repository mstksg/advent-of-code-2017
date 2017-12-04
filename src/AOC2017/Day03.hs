
{-# LANGUAGE ViewPatterns #-}

module AOC2017.Day03 (day03a, day03b) where

import           AOC2017.Types              (Challenge)
import           Control.Monad              ((>=>))
import           Control.Monad.Trans.State  (State, state, evalState)
import           Control.Monad.Trans.Writer (Writer, writer, execWriter)
import           Data.List                  (find)
import           Data.Maybe                 (mapMaybe, fromJust)
import           Data.Semigroup             (Semigroup(..), Sum(..))
import qualified Data.Map                   as M

-- Why aren't these defined in base by default?
data EndoKleisli m a = EK { runEK :: a -> m a }
instance Monad m => Semigroup (EndoKleisli m a) where
    f <> g = EK (runEK f >=> runEK g)
instance Monad m => Monoid (EndoKleisli m a) where
    mempty  = EK return
    mappend = (<>)

type Trail a = EndoKleisli (Writer [a]) a

type Point = (Sum Int, Sum Int)

norm :: Point -> Int
norm (Sum x, Sum y) = abs x + abs y

move :: Point -> Trail Point
move p = EK $ \p0 -> writer (p0 <> p, [p0 <> p])

spiral :: Trail Point
spiral = move (0,0)
      <> foldMap loop [1..]
  where
    loop :: Int -> Trail Point
    loop n = stimes (2*n-1) (move ( 1, 0))
          <> stimes (2*n-1) (move ( 0, 1))
          <> stimes (2*n  ) (move (-1, 0))
          <> stimes (2*n  ) (move ( 0,-1))

ulam :: [Point]
ulam = execWriter $ runEK spiral (0,0)

day03a :: Challenge
day03a (read->i) = show . norm $ ulam !! (i - 1)

updateMap :: Point -> State (M.Map Point Int) Int
updateMap p = state $ \m0 ->
    let newPoint = sum . mapMaybe (`M.lookup` m0) $
          [ p <> (Sum x, Sum y) | x <- [-1 .. 1]
                                , y <- [-1 .. 1]
                                , x /= 0 || y /= 0
                                ]
    in  (newPoint, M.insertWith (const id) p newPoint m0)

cellNums :: [Int]
cellNums = flip evalState (M.singleton (0, 0) 1) $
    traverse updateMap ulam

day03b :: Challenge
day03b (read->i) = show . fromJust $ find (> i) cellNums
