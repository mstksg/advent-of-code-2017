module AOC2017.Day25 (day25a, day25b) where

import           AOC2017.Types              (Challenge)
import           AOC2017.Util
import           AOC2017.Util.Tape
import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Foldable
import qualified Data.IntMap                as IM
import qualified Data.IntSet                as IS
import qualified Data.Map                   as M
import qualified Data.Set                   as S
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P

data St = SA | SB | SC | SD | SE | SF

data Dir = DL | DR

type Rule = (Bool, Dir, St)

type TapeState = (Tape Bool, St)


rule :: St -> (Rule, Rule)
rule = \case
    SA -> ((True, DR, SB), (False, DR, SF))
    SB -> ((False,DL, SB), (True, DL, SC))
    SC -> ((True,DL,SD),(False, DR, SC))
    SD -> ((True, DL, SE),(True, DR, SA))
    SE -> ((True, DL, SF), (False, DL, SD))
    SF -> ((True, DR, SA), (False, DL, SE))

step :: TapeState -> TapeState
step (!t0, !st0) = (t1, st1)
  where
    t1 = t0 & tFocus .~ newFoc
            & case dirTurn of
                DL -> moveLeftD False
                DR -> moveRightD False
    (newFoc, dirTurn, st1)
      | t0 ^. tFocus = snd (rule st0)
      | otherwise    = fst (rule st0)

day25a :: Challenge
day25a _ = show . length . filter id . toList . fst
         $ iterate step (Tape [] False [], SA ) !!! 12964419

day25b :: Challenge
day25b = undefined
