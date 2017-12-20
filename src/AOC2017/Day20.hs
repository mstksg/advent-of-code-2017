module AOC2017.Day20 (day20a, day20b) where

import           AOC2017.Types        (Challenge)
import           AOC2017.Util         ((!!!))
import           Control.Applicative  (liftA2)
import           Control.Lens         ((^..), _1, _2, _3, to)
import           Control.Monad        (mfilter)
import           Data.Foldable        (toList)
import           Data.Void            (Void)
import qualified Data.Map             as M
import qualified Data.Set             as S
import qualified Data.Vector          as V
import qualified Linear               as L
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P

data S = S { _sPos :: !(V.Vector (Maybe (L.V3 Int)))
           , _sVel :: !(V.Vector (Maybe (L.V3 Int)))
           , _sAcc :: !(V.Vector (Maybe (L.V3 Int)))
           }
  deriving Show

step :: S -> S
step s@S{..} = s { _sVel = v'
                 , _sPos = (V.zipWith . liftA2) (+) _sPos v'
                 }
  where
    v' = (V.zipWith . liftA2) (+) _sVel _sAcc

collide :: S -> S
collide s@S{..} = s { _sPos = mfilter (`S.notMember` collisions) <$> _sPos }
  where
    collisions = M.keysSet
               . M.filter @Int (> 1)
               . M.fromListWith (+)
               . fmap (,1)
               $ foldMap toList _sPos

dists :: S -> V.Vector (Maybe Int)
dists = (fmap . fmap) (sum . fmap abs) . _sPos

parse :: String -> S
parse = either (error . show) id . P.runParser parsePoints ""

day20a :: Challenge
day20a = show . (!!! 1000) . map V.minIndex
       . scanl1 ((V.zipWith . liftA2) (+)) . map dists
       . iterate step
       . parse

day20b :: Challenge
day20b = show . length . (!!! 1000) . map (foldMap toList . _sPos)
       . iterate (collide . step)
       . parse


-- * Parsers

type Parser = P.Parsec Void String

parseVector :: Parser (L.V3 Int)
parseVector = do
    _ <- P.anyChar  *> P.char '='
    x <- P.char '<' *> numParser
    y <- P.char ',' *> numParser
    z <- P.char ',' *> numParser
    _ <- P.char '>'
    return $ L.V3 x y z
  where
    numParser = do
      neg <- maybe id (const negate) <$> P.optional (P.string "-")
      digits <- P.many P.digitChar
      return . neg $ read digits

parseLine :: Parser (L.V3 Int, L.V3 Int, L.V3 Int)
parseLine = do
    [pos, vel, acc] <- parseVector `P.sepBy` P.string ", "
    return (pos, vel, acc)

parsePoints :: Parser S
parsePoints = do
    pts <- P.many (parseLine <* P.try P.newline)
    return $ S (V.fromList (pts ^.. traverse . _1 . to Just))
               (V.fromList (pts ^.. traverse . _2 . to Just))
               (V.fromList (pts ^.. traverse . _3 . to Just))

