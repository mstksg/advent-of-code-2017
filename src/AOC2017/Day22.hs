{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module AOC2017.Day22 (day22a, day22b) where

import           AOC2017.Types             (Challenge)
import           Control.Lens              (makeClassy, use, at, non, zoom, (+=), (<<>=))
import           Control.Monad             (replicateM)
import           Control.Monad.Trans.State (State, state, evalState)
import qualified Data.Map                  as M
import qualified Linear                    as L

data Flag = FC | FW | FI | FF
  deriving Eq

data Dir = N | E | S | W
  deriving Enum

instance Monoid Dir where
    -- | identity rotation
    mempty      = N
    -- | compose rotations
    mappend h t = toEnum $ (fromEnum h + fromEnum t) `mod` 4

data St = MkSt { _sWorld :: !(M.Map (L.V2 Int) Flag)
               , _sPos   :: !(L.V2 Int)
               , _sDir   :: !Dir
               }
makeClassy ''St

delta :: Dir -> L.V2 Int
delta = \case
    N -> L.V2 0 1
    E -> L.V2 1 0
    S -> L.V2 0 (-1)
    W -> L.V2 (-1) 0

-- | Lift a 'State Flag Dir' (modify a Flag and produce a direction change)
-- to a 'State St Flag' (modify the simulation state and produce a new
-- Flag)
step
    :: State Flag Dir
    -> State St Flag
step stF = do
    p      <- use sPos
    turn   <- zoom (sWorld . at p . non FC) stF
    newDir <- sDir <<>= turn
    sPos   += delta newDir
    use (sWorld . at p . non FC)

day22 :: Int -> State Flag Dir -> M.Map (L.V2 Int) Flag -> Int
day22 n stF w0 = length . filter (== FI)
               $ evalState (replicateM n (step stF)) st0
  where
    st0 = MkSt w0 p0 N
    p0  = (`div` 2) <$> fst (M.findMax w0)

day22a :: Challenge
day22a = show . day22 1e4 partA . parse
  where
    partA :: State Flag Dir
    partA = state $ \case
      FC -> (W, FI)
      FI -> (E, FC)
      _  -> error "Shouldn't happen"

day22b :: Challenge
day22b = show . day22 1e7 partB . parse
  where
    partB :: State Flag Dir
    partB = state $ \case
      FC -> (W, FW)
      FW -> (N, FI)
      FI -> (E, FF)
      FF -> (S, FC)

parse :: String -> M.Map (L.V2 Int) Flag
parse = M.unions . zipWith mkRow [0..] . reverse . lines
  where
    fl = \case '#' -> FI
               _   -> FC
    mkRow y = M.fromList . zip ixes . map fl
      where
        ixes = [ L.V2 x y | x <- [0..] ]
