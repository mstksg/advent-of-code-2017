module AOC2017.Day25 (day25a) where

import           AOC2017.Types   (Challenge)
import           AOC2017.Util    ((!!!))
import           Control.Lens.At (Contains(..))
import           Data.Bifunctor  (first)
import           Data.Coerce     (coerce)
import           Data.Semigroup  (Semigroup(..), Last(..), Sum(..))
import qualified Data.IntSet     as IS
import qualified Data.Map        as M

type St      = Last Char
type Step    = (Sum Int, St)
type RuleMap = M.Map St ((Step, Bool), (Step, Bool))

runRule :: RuleMap -> St -> Bool -> (Step, Bool)
runRule rm st = \case
    False -> fst $ rm M.! st
    True  -> snd $ rm M.! st

step :: RuleMap -> Step -> IS.IntSet -> (Step, IS.IntSet)
step rm s0@(Sum i,st) = first (s0 <>) . contains i (runRule rm st)


day25a :: Challenge
day25a _ = show . IS.size . snd
         . (!!! 12964419)
         . iterate (uncurry (step ruleMap))
         $ ((0, Last 'A'), IS.empty)

ruleMap :: RuleMap
ruleMap = M.fromList (coerce ruleList)
  where
    ruleList :: [(Char, (((Int, Char), Bool), ((Int, Char), Bool)))]
    ruleList = [ ('A', ((( 1, 'B'), True ), (( 1, 'F'), False)))
               , ('B', (((-1, 'B'), False), ((-1, 'C'), True )))
               , ('C', (((-1, 'D'), True ), (( 1, 'C'), False)))
               , ('D', (((-1, 'E'), True ), (( 1, 'A'), True )))
               , ('E', (((-1, 'F'), True ), ((-1, 'D'), False)))
               , ('F', ((( 1, 'A'), True ), ((-1, 'E'), False)))
               ]

