module AOC2017.Day19 (day19a, day19b) where

import           AOC2017.Types             (Challenge)
import           Control.Applicative       (many)
import           Control.Monad             (guard)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State (StateT, evalStateT, get, put)
import           Data.Char                 (isAlpha)
import qualified Data.Vector               as V
import qualified Linear                    as L

type Grid  = V.Vector (V.Vector Char)
type Point = L.V2 Int

follow :: Grid -> StateT (Point, Point) [] Char
follow g = do
    -- (last position, current position)
    (x0, x1) <- get
    x2 <- lift $ case gridAt x1 of
        ' ' -> []
        '+' -> (+ x1) <$> [ L.V2 0    1
                          , L.V2 0    (-1)
                          , L.V2 1    0
                          , L.V2 (-1) 0
                          ]
        _   -> [ x1 + (x1 - x0) ]
    let nextChar = gridAt x2
    guard $ x2 /= x0
    guard $ inBounds x2
    guard $ nextChar `elem` "|-+" || isAlpha nextChar
    put (x1, x2)
    return nextChar
  where
    gridAt   (L.V2 x y) = g V.! y V.! x
    inBounds (L.V2 x y) = all ($ x) [(>= 0), (< V.length (g V.! 0))]
                       && all ($ y) [(>= 0), (< V.length g        )]

day19 :: Grid -> String
day19 g = ('|':) . head . flip evalStateT p0 . many . follow $ g
  where
    p0 = (L.V2 x0 (-1), L.V2 x0 0)
    Just x0 = V.elemIndex '|' (g V.! 0)

day19a :: Challenge
day19a = filter isAlpha . day19 . parse

day19b :: Challenge
day19b = show . length . day19 . parse

parse :: String -> V.Vector (V.Vector Char)
parse = V.fromList . map V.fromList . lines

