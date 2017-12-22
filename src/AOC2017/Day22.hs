{-# LANGUAGE RecordWildCards #-}

module AOC2017.Day22 (day22a, day22b) where

import           AOC2017.Types (Challenge)
import           Debug.Trace
import           AOC2017.Util
import           Control.Lens
import qualified Data.Map      as M
import qualified Linear        as L

type World = M.Map (L.V2 Int) Bool

data Dir = N | E | S | W

data Flag = FC | FW | FI | FF
  deriving Eq

data S = St { _sWorld :: M.Map (L.V2 Int) Bool
            , _sVPos  :: L.V2 Int
            , _sVDir  :: Dir
            , _sInfec :: Int
            }

cycFlag = \case
    FC -> FW
    FW -> FI
    FI -> FF
    FF -> FC

turnRight = \case
    N -> E
    E -> S
    S -> W
    W -> N

turnLeft = turnRight . turnRight . turnRight

delt = \case
    N -> L.V2 0 1
    E -> L.V2 1 0
    S -> L.V2 0 (-1)
    W -> L.V2 (-1) 0

step :: S -> S
step St{..} = St w' p' d' i'
  where
    currPos = _sWorld ^. at _sVPos . non False
    d' | currPos   = turnRight _sVDir
       | otherwise = turnLeft _sVDir
    p' = _sVPos + delt d'
    i' | currPos = _sInfec
       | otherwise = _sInfec + 1
    w' = _sWorld & at _sVPos . non False %~ not

parse :: String -> M.Map (L.V2 Int) Bool
parse = M.unions . map g . zip [0..] . reverse . lines
  where
    g (i, w) = M.fromList . map h . zip [0..] $ ((== '#') <$> w)
      where
        h (j, c) = (L.V2 j i, c)

day22a :: Challenge
day22a (parse->w) = show . _sInfec $ iterate step (St w mid N 0) !!! 1e4
  where
    mid = fmap (`div` 2) . fst $ M.findMax w

data S2 = St2 { _s2World :: M.Map (L.V2 Int) Flag
              , _s2VPos  :: L.V2 Int
              , _s2VDir  :: Dir
              , _s2Infec :: Int
              }

parse2 :: String -> M.Map (L.V2 Int) Flag
parse2 = M.unions . map g . zip [0..] . reverse . lines
  where
    fl '.' = FC
    fl '#' = FI
    g (i, w) = M.fromList . map h . zip [0..] $ (fl <$> w)
      where
        h (j, c) = (L.V2 j i, c)

step2 :: S2 -> S2
step2 St2{..} = St2 w' p' d' i'
  where
    currPos = _s2World ^. at _s2VPos . non FC
    d' = _s2VDir & case currPos of
                     FC -> turnLeft
                     FW -> id
                     FI -> turnRight
                     FF -> turnRight . turnRight
    p' = _s2VPos + delt d'
    i' = case currPos of
           FW -> _s2Infec + 1
           _  -> _s2Infec
    w' = _s2World & at _s2VPos . non FC %~ cycFlag

day22b :: Challenge
day22b (parse2->w) = show . _s2Infec $ iterate step2 (St2 w mid N 0) !!! 1e7
  where
    mid = fmap (`div` 2) . fst $ M.findMax w

