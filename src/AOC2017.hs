
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
    [ (1, M.fromList [('a', day01a)
                     ,('b', day01b)])
    , (2, M.fromList [('a', day02a)
                     ,('b', day02b)])
    , (3, M.fromList [('a', day03a)
                     ,('b', day03b)])
    , (4, M.fromList [('a', day04a)
                     ,('b', day04b)])
    , (5, M.fromList [('a', day05a)
                     ,('b', day05b)])
    , (6, M.fromList [('a', day06a)
                     ,('b', day06b)])
    , (7, M.fromList [('a', day07a)
                     ,('b', day07b)])
    , (8, M.fromList [('a', day08a)
                     ,('b', day08b)])
    , (9, M.fromList [('a', day09a)
                     ,('b', day09b)])
    , (10, M.fromList [('a', day10a)
                      ,('b', day10b)])
    , (11, M.fromList [('a', day11a)
                      ,('b', day11b)])
    , (12, M.fromList [('a', day12a)
                      ,('b', day12b)])
    ]

