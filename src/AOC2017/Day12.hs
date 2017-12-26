module AOC2017.Day12 (day12a, day12b) where

import           AOC2017.Types          (Challenge)
import           AOC2017.Util.Disjoints (Disjoints(..), disjoint)
import           Data.Char              (isDigit)
import           Data.List              (find)
import           Data.Maybe             (fromJust)
import qualified Data.IntSet            as IS
import qualified Data.Set               as S

parseLine :: String -> IS.IntSet
parseLine (words->n:_:ns) = IS.fromList $ read n
                                        : map (read . filter isDigit) ns
parseLine _               = error "No parse"

build :: String -> Disjoints
build = foldMap (disjoint . parseLine) . lines

day12a :: Challenge
day12a = show . IS.size . fromJust . find (0 `IS.member`)
       . getD . build

day12b :: Challenge
day12b = show . S.size
       . getD . build
