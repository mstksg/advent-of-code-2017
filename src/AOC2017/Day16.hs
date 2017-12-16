module AOC2017.Day16 where
-- module AOC2017.Day16 (day16a, day16b) where

import           AOC2017.Types   (Challenge)
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Semigroup
import qualified Data.Map        as M
import qualified Data.Vector     as V

data Dance = DP { _dPos  :: !(M.Map Int Int)
                , _dName :: !(M.Map Char Char)
                }

instance Semigroup Dance where
    DP p1 n1 <> DP p2 n2 = DP (p1 `comp` p2) (n1 `comp` n2)
      where
        comp m1 m2 = (follow <$> m1) <> m2
          where
            follow i = M.findWithDefault i i m2
instance Monoid Dance where
    mappend = (<>)
    mempty = DP M.empty M.empty

runDance :: Dance -> String
runDance d = go <$> ['a' .. 'p']
  where
    go c = let cIx = ord (M.findWithDefault c c (_dName d)) - oA
           in  chr $ M.findWithDefault cIx cIx (_dPos d) + oA
    oA = ord 'a'

parse :: String -> Dance
parse = foldMap parseMove . splitOn ","
  where
    parseMove :: String -> Dance
    parseMove = \case
      's':(read->n)            -> DP (rotator n)                 M.empty
      'x':(splitOn "/"->n:m:_) -> DP (swapper (read n) (read m)) M.empty
      'p':n:_:m:_              -> DP M.empty                     (swapper n m)
      _                        -> error "No parse"
    rotator :: Int -> M.Map Int Int
    rotator n = M.fromList [ (i, (i - n) `mod` 16) | i <- [0..15] ]
    swapper :: Ord a => a -> a -> M.Map a a
    swapper x y = M.fromList [ (x, y), (y, x) ]


-- data Move = MSpin     Int
--           | MExchange Int  Int
--           | MPartner  Char Char

-- step :: V.Vector Char -> Move -> V.Vector Char
-- step v0 = \case
--   MSpin n       -> let (x,y) = V.splitAt (16 - n) v0
--                    in  y V.++ x
--   MExchange n m -> v0 V.// [(n, v0 V.! m),(m, v0 V.! n)]
--   MPartner n m  -> let Just iN = V.findIndex (== n) v0
--                        Just iM = V.findIndex (== m) v0
--                    in  v0 V.// [(iN, m),(iM, n)]

-- parse :: String -> [Move]
-- parse = map parseMove . splitOn ","
--   where
--     parseMove :: String -> Move
--     parseMove = \case
--       's':(read->n)            -> MSpin n
--       'x':(splitOn "/"->n:m:_) -> MExchange (read n) (read m)
--       'p':n:_:m:_              -> MPartner n m
--       _                        -> error "No parse"

-- initial :: V.Vector Char
-- initial = V.fromList ['a' .. 'p']

day16a :: Challenge
day16a = runDance . parse
day16b = runDance . parse

-- day16b :: Challenge
-- day16b (parse->moves) = V.toList $
--     iterate oneCycle initial !! leftovers
--   where
--     oneCycle xs = foldl' step xs moves
--     loopLength = (+ 1)
--                . length
--                . takeWhile (/= initial)
--                . drop 1
--                . iterate oneCycle
--                $ initial
--     leftovers = 1e9 `mod` loopLength
