{-# LANGUAGE FlexibleContexts #-}

module AOC2017.Day20 (day20a, day20b) where

import           AOC2017.Types                 (Challenge)
import           AOC2017.Util                  ((!!!))
import           Control.Applicative           (liftA2)
import           Control.Lens                  ((^..), _1, _2, _3, to)
import           Control.Monad                 (mfilter)
import           Data.Char                     (isDigit)
import           Data.Foldable                 (toList)
import qualified Data.Map                      as M
import qualified Data.Set                      as S
import qualified Data.Vector                   as V
import qualified Linear                        as L

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

day20a :: Challenge
day20a = show . (!!! 1000) . map V.minIndex
       . scanl1 ((V.zipWith . liftA2) (+)) . map dists
       . iterate step
       . parse

day20b :: Challenge
day20b = show . length . (!!! 1000)
       . map (foldMap toList . _sPos)
       . iterate (collide . step)
       . parse


-- * Parsers

parse :: String -> S
parse = mkS . map parseLine . lines
  where
    mkS pts = S (V.fromList (pts ^.. traverse . _1 . to Just))
                (V.fromList (pts ^.. traverse . _2 . to Just))
                (V.fromList (pts ^.. traverse . _3 . to Just))

parseLine :: String -> (L.V3 Int, L.V3 Int, L.V3 Int)
parseLine (map read.words.onlyNums->[pX,pY,pZ,vX,vY,vZ,aX,aY,aZ])
            = (L.V3 pX pY pZ, L.V3 vX vY vZ, L.V3 aX aY aZ)
parseLine _ = error "No parse"

onlyNums :: String -> String
onlyNums = map $ \c -> if isDigit c || c == '-'
                         then c
                         else ' '
