
module AOC2017 (
    module AOC
  , challengeMap
  ) where

import           AOC2017.Day01 as AOC
import           AOC2017.Day02 as AOC
import           AOC2017.Day03 as AOC
import           AOC2017.Day04 as AOC
import           AOC2017.Day05 as AOC
import           AOC2017.Day06 as AOC
import           AOC2017.Day07 as AOC
import           AOC2017.Day08 as AOC
import           AOC2017.Day09 as AOC
import           AOC2017.Day10 as AOC
import           AOC2017.Day11 as AOC
import           AOC2017.Day12 as AOC
import           AOC2017.Types as AOC

import qualified Data.IntMap   as IM
import qualified Data.Map      as M

challengeMap :: IM.IntMap (M.Map Char Challenge)
challengeMap = IM.fromList
    [ (d, M.fromList [('a', ca),('b', cb)])
    | (d, (ca, cb)) <- challenges
    ]

challenges :: [(Int, (Challenge, Challenge))]
challenges = [ ( 1, (day01a, day01b))
             , ( 2, (day02a, day02b))
             , ( 3, (day03a, day03b))
             , ( 4, (day04a, day04b))
             , ( 5, (day05a, day05b))
             , ( 6, (day06a, day06b))
             , ( 7, (day07a, day07b))
             , ( 8, (day08a, day08b))
             , ( 9, (day09a, day09b))
             , (10, (day10a, day10b))
             , (11, (day11a, day11b))
             , (12, (day12a, day12b))
             ]

