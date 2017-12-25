module AOC2017.Day25 (day25a, day25b) where

import           AOC2017.Types (Challenge)
import           AOC2017.Util
import           Control.Lens
import qualified Data.IntSet   as IS

data St = SA | SB | SC | SD | SE | SF

data Dir = DL | DR

type Rule = (Bool, Dir, St)

type TapeState = (Int, IS.IntSet, St)

rule :: St -> (Rule, Rule)
rule = \case
    SA -> ((True, DR, SB), (False, DR, SF))
    SB -> ((False,DL, SB), (True, DL, SC))
    SC -> ((True,DL,SD),(False, DR, SC))
    SD -> ((True, DL, SE),(True, DR, SA))
    SE -> ((True, DL, SF), (False, DL, SD))
    SF -> ((True, DR, SA), (False, DL, SE))

step :: TapeState -> TapeState
step (!i0, !t0, !st0) = (i1, t1, st1)
  where
    i1 = case dirTurn of
           DL -> i0 - 1
           DR -> i0 + 1
    t1 = t0 & if newFoc
                then IS.insert i0
                else IS.delete i0
    (newFoc, dirTurn, st1)
      | i0 `IS.member` t0 = snd (rule st0)
      | otherwise        = fst (rule st0)

day25a :: Challenge
day25a _ = show . IS.size . view _2
         $ iterate step (0, IS.empty, SA) !!! 12964419

day25b :: Challenge
day25b _ = "Merry Christmas!"
