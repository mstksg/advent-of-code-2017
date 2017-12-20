{-# LANGUAGE FlexibleContexts #-}

module AOC2017.Day20 (day20a, day20b) where

import           AOC2017.Types       (Challenge)
import           AOC2017.Util        ((!!!))
import           Control.Applicative (liftA2)
import           Control.Monad       (mfilter)
import           Data.Char           (isDigit)
import           Data.Foldable       (toList)
import           Data.List.Split     (splitOn)
import qualified Data.Map            as M
import qualified Data.Set            as S
import qualified Data.Vector         as V
import qualified Linear              as L

data S = S { _sPos :: !(V.Vector (Maybe (L.V3 Int)))
           , _sVel :: !(V.Vector (Maybe (L.V3 Int)))
           , _sAcc :: !(V.Vector (Maybe (L.V3 Int)))
           }
  deriving Show

step :: S -> S
step s@S{..} = s { _sPos = p, _sVel = v }
  where
    [_,v,p] = scanl1 ((V.zipWith . liftA2) (+)) [_sAcc, _sVel, _sPos]

collide :: S -> S
collide s@S{..} = s { _sPos = mfilter (`S.notMember` collisions) <$> _sPos }
  where
    collisions = M.keysSet . M.filter @Int (> 1)
               . M.fromListWith (+) . fmap (,1)
               $ foldMap toList _sPos

norm :: L.V3 Int -> Int
norm = sum . fmap abs

day20a :: Challenge
day20a = show . V.minIndex
       . (fmap . fmap . fmap) norm
       . (\case S r v a -> V.zipWith3 L.V3 a v r)
       . parse

day20b :: Challenge
day20b = show . length . (!!! 1000)
       . map (foldMap toList . _sPos)
       . iterate (collide . step)
       . parse

parse :: String -> S
parse = (\case L.V3 r v a -> S r v a)
      . traverse (fmap Just . parseLine)
      . V.fromList . lines

parseLine :: String -> L.V3 (L.V3 Int)
parseLine (map(read.filter numChar).splitOn","->[pX,pY,pZ,vX,vY,vZ,aX,aY,aZ])
            = L.V3 (L.V3 pX pY pZ) (L.V3 vX vY vZ) (L.V3 aX aY aZ)
parseLine _ = error "No parse"

numChar :: Char -> Bool
numChar c = isDigit c || c == '-'
