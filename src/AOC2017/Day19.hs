module AOC2017.Day19 (day19a, day19b) where

import           AOC2017.Types             (Challenge)
import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Char
import           Data.Maybe
import qualified Data.Vector               as V
import qualified Linear                    as L

type Grid = V.Vector (V.Vector Char)
type Point = L.V2 Int

follow :: Grid -> StateT (L.V2 Point) Maybe (Maybe Char)
follow g = do
    L.V2 x0 (x1@(L.V2 x y)) <- get
    let nextSteps = case g V.! y V.! x of
          '+' -> listToMaybe $ search x0 x1
          ' ' -> Nothing
          c   -> Just (x1 + (x1 - x0), c <$ guard (isAlpha c))
    case nextSteps of
      Nothing        -> empty
      Just (x2, out) -> out <$ put (L.V2 x1 x2)
  where
    search x0 x1 = do
      p@(L.V2 x y) <- (+ x1) <$> [ L.V2 0    1
                                 , L.V2 0    (-1)
                                 , L.V2 1    0
                                 , L.V2 (-1) 0
                                 ]
      guard $ p /= x0 && valid p
      let c = g V.! y V.! x
      guard $ (c `elem` "|-") || isAlpha c
      return (p, Nothing)
    valid (L.V2 x y) = all ($ x) [(>= 0), (< V.length (g V.! 0))]
                    && all ($ y) [(>= 0), (< V.length g        )]

day19 :: Grid -> [Maybe Char]
day19 g = fromJust . flip evalStateT p0 . many . follow $ g
  where
    p0 = L.V2 (L.V2 x0 (-1)) (L.V2 x0 0)
    Just x0 = V.elemIndex '|' (g V.! 0)

day19a :: Challenge
day19a = catMaybes . day19 . parse

day19b :: Challenge
day19b = show . length . day19 . parse

parse :: String -> V.Vector (V.Vector Char)
parse = V.fromList . map V.fromList . lines

