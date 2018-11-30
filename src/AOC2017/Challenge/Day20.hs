-- module AOC2017.Day20 (day20a, day20b) where
module AOC2017.Challenge.Day20 where

import           AOC2017.Types   (Challenge)
import           AOC2017.Util    (scanlT)
import           Data.Char       (isDigit)
import           Data.Foldable   (toList)
import           Data.List       (find)
import           Data.List.Split (splitOn)
import           Data.Maybe      (fromJust)
import qualified Data.Map        as M
import qualified Data.Set        as S
import qualified Data.Vector     as V
import qualified Linear          as L

type Point = L.V3 Int

data Particle a = P { _pAcc :: !a
                    , _pVel :: !a
                    , _pPos :: !a
                    }
  deriving (Functor, Foldable, Traversable, Show, Eq, Ord)

type System = [Particle Point]

step :: Num a => Particle a -> Particle a
step = scanlT (+) 0

collide :: System -> System
collide s0 = filter ((`S.notMember` collisions) . _pPos) s0
  where
    collisions :: S.Set Point
    collisions = M.keysSet . M.filter @Int (> 1)
               . M.fromListWith (+)
               . map ((,1) . _pPos)
               $ toList s0

norm :: Point -> Int
norm = sum . fmap abs

day20a :: Challenge
day20a = show . V.minIndex . V.fromList
       . (map . fmap) norm
       . parse

day20b :: Challenge
day20b = show . length . fromJust . find stop
       . iterate (collide . map step)
       . parse
  where
    stop = (> 1000) . minimum . map (norm . _pPos)

parse :: String -> System
parse = map parseLine . lines
  where
    parseLine :: String -> Particle Point
    parseLine (map(read.filter num).splitOn","->[pX,pY,pZ,vX,vY,vZ,aX,aY,aZ])
                = P { _pAcc = L.V3 aX aY aZ
                    , _pVel = L.V3 vX vY vZ
                    , _pPos = L.V3 pX pY pZ
                    }
    parseLine _ = error "No parse"
    num :: Char -> Bool
    num c = isDigit c || c == '-'
