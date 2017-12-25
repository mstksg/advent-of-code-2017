module AOC2017.Day25 (day25a) where

import           AOC2017.Types       (Challenge)
import           AOC2017.Util        ((!!!))
import           Control.Lens hiding ((<>~))
import           Data.Coerce
import           Data.Semigroup
import qualified Data.IntSet         as IS
import qualified Data.Map            as M

type St      = Last Char
type Step    = (Sum Int, St)
type RuleMap = M.Map St ((Step, Bool), (Step, Bool))

runRule :: RuleMap -> St -> Bool -> (Step, Bool)
runRule rm st = \case
    False -> fst $ rm M.! st
    True  -> snd $ rm M.! st

step :: RuleMap -> (Step, IS.IntSet) -> (Step, IS.IntSet)
step rm (s0@(Sum i,st),t) = t & contains i %%~ runRule rm st
                              & _1          %~ (s0 <>)

day25a :: Challenge
day25a _ = show . IS.size . snd
         . (!!! 12964419)
         . iterate (step ruleMap)
         $ ((0, Last 'a'), mempty)

ruleMap :: RuleMap
ruleMap = M.fromList (coerce ruleList)
  where
    ruleList :: [(Char, (((Int, Char), Bool), ((Int, Char), Bool)))]
    ruleList = [ ('a', ((( 1, 'b'), True ), (( 1, 'f'), False)))
               , ('b', (((-1, 'b'), False), ((-1, 'c'), True )))
               , ('c', (((-1, 'd'), True ), (( 1, 'c'), False)))
               , ('d', (((-1, 'e'), True ), (( 1, 'a'), True )))
               , ('e', (((-1, 'f'), True ), ((-1, 'd'), False)))
               , ('f', ((( 1, 'a'), True ), ((-1, 'e'), False)))
               ]

