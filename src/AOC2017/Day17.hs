module AOC2017.Day17 (day17a, day17b) where

import           AOC2017.Types      (Challenge)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe         (mapMaybe)
import qualified Data.List.NonEmpty as NE

(!!!) :: [a] -> Int -> a
[] !!! _ = error "Out of range"
(x:_ ) !!! 0 = x
(x:xs) !!! n = x `seq` (xs !!! (n - 1))

data Tape a = Tape { _tLefts  :: ![a]
                   , _tFocus  :: !a
                   , _tRights :: ![a]
                   }
  deriving Show

move :: Tape a -> Tape a
move (Tape ls x rs) = case rs of
    [] -> let l :| ls' = NE.reverse (x :| ls)
          in  Tape [] l ls'
    r:rs' -> Tape (x:ls) r rs'

unshift :: a -> Tape a -> Tape a
unshift y (Tape ls x rs) = Tape (x:ls) y rs

run :: Int -> [Tape Int]
run n = go 1 (Tape [] 0 [])
  where
    go x t0 = t0 : go (x + 1) t1
      where
        t1 = unshift x $ iterate move t0 !!! n

day17a :: Challenge
day17a (read->n) = show r
  where
    Tape _ _ (r:_) = run n !!! 2017

day17b :: Challenge
day17b (read->n) = show . last
                 . mapMaybe (\(x, p) -> [ x | p == 1 ])
                 . take 5e7
                 $ zip @Int @Int [0 ..] insertionPoints
  where
    insertionPoints :: [Int]
    insertionPoints = scanl go 0 [1..]
      where
        go i x = ((i + n) `mod` x) + 1
