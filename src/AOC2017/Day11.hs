{-# LANGUAGE LambdaCase #-}

module AOC2017.Day11 (day11a, day11b) where

import           AOC2017.Types                         (Challenge)
import           Data.Char                             (isAlpha)
import           Data.List                             (foldl')
import           Data.List.Split                       (splitOn)
import           Data.Maybe                            (fromJust)
import           Math.Geometry.Grid                    (distance, neighbour)
import           Math.Geometry.Grid.Hexagonal2         (UnboundedHexGrid(..))
import           Math.Geometry.Grid.HexagonalInternal2 (HexDirection(..))

parse :: String -> [HexDirection]
parse = map (parseDir . filter isAlpha) . splitOn ","
  where
    parseDir = \case
      "nw" -> Northwest
      "n"  -> North
      "ne" -> Northeast
      "se" -> Southeast
      "s"  -> South
      "sw" -> Southwest
      d    -> error $ "Bad direction " ++ d

step :: (Int, Int) -> HexDirection -> (Int, Int)
step p = fromJust . neighbour UnboundedHexGrid p

day11a :: Challenge
day11a = show . distance UnboundedHexGrid (0,0)
       . foldl' step (0,0)
       . parse

day11b :: Challenge
day11b = show . maximum . map (distance UnboundedHexGrid (0,0))
       . scanl step (0,0)
       . parse
