-- module AOC2017.Day12 (day12a, day12b) where

{-# LANGUAGE ViewPatterns #-}
module AOC2017.Day12 where

import           AOC2017.Types  (Challenge)
import           Data.Char
import           Data.List
import qualified Data.Set       as S

parse :: String -> S.Set Int
parse (words->n:_:ns) = S.fromList $ read n
                                   : (read . filter isDigit <$> ns)
parse _ = error "No parse"

pass :: S.Set Int -> [S.Set Int] -> S.Set Int
pass = foldl' go
  where
    go s1 s2 | null (s1 `S.intersection` s2) = s1
             | otherwise                     = s1 `S.union` s2

untilEq :: Eq a => (a -> b -> a) -> a -> b -> a
untilEq f x y | z == x    = z
              | otherwise = untilEq f z y
  where
    z = f x y

day12a :: Challenge
day12a = show . S.size . untilEq pass (S.singleton 0) . map parse . lines

day12b :: Challenge
day12b inp = show
           . S.size
           . S.map (\i -> untilEq pass (S.singleton i) inpList)
           $ allNums
  where
    inpList = parse <$> lines inp
    allNums :: S.Set Int
    allNums = S.unions inpList
