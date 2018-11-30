module AOC2017.Challenge.Day17 (day17a, day17b) where

import           AOC2017.Types
import           Control.Lens
import           Data.List                      (elemIndices, foldl')
import           Text.Read                      (readMaybe)
import qualified Data.List.PointedList.Circular as PL

step :: Int -> PL.PointedList a -> a -> PL.PointedList a
step n t0 x = PL.insert x . PL.moveN n $ t0

day17a :: Challenge
day17a = runC C
    { cParse = readMaybe
    , cShow  = show @Int
    , cSolve = \n -> Just . view PL.focus . PL.next
                   . foldl' (step n) (PL.singleton 0)
                   $ [1 .. 2017]
    }

day17b :: Challenge
day17b = runC C
    { cParse = readMaybe
    , cShow  = show
    , cSolve = \n -> let jump i x = ((i + n) `mod` x) + 1
                     in  preview _last
                       . elemIndices @Int 1
                       $ scanl jump 0 [1 .. 5e7]
    }
