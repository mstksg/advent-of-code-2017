module AOC2017.Day16 (day16a, day16b) where

-- import qualified Numeric.LinearAlgebra.Static as H
import           AOC2017.Types                   (Challenge)
import           Control.Lens
import           Data.Char
import           Data.List                       (foldl')
import           Data.List.Split                 (splitOn)
import           Data.Proxy
import           Data.Semigroup
import           GHC.TypeNats
import           Numeric.Natural
import qualified Data.Vector                     as V
import qualified Data.Vector.Storable            as VS
import qualified Numeric.LinearAlgebra           as H

data Dance = D { _dPosPerm  :: H.Matrix Double
               , _dNamePerm :: H.Matrix Double
               }
instance Semigroup Dance where
    D p1 n1 <> D p2 n2 = D (p1 H.<> p2) (n1 H.<> n2)
instance Monoid Dance where
    mappend = (<>)
    mempty = D (H.ident 16) (H.ident 16)

runDance :: Dance -> String
runDance d = V.toList . runNamePerm . runPosPerm $ V.fromList ['a' .. 'p']
  where
    runNamePerm = 

permVec :: [Int] -> H.Matrix Double
permVec (take 16->v) = H.ident 16 H.?? (H.PosCyc (H.idxs v), H.All)

spin :: Int -> Dance
spin n = mempty { _dPosPerm = permVec . drop (16 - n) . cycle $ [0..15]
                }
exch :: Int -> Int -> Dance
exch i j = mempty { _dPosPerm = permVec $ [0..15] & ix i .~ j
                                                  & ix j .~ i
                  }
part :: Char -> Char -> Dance
part i j = mempty { _dNamePerm = permVec $ [0..15] & ix i' .~ j'
                                                   & ix j' .~ i'
                  }
  where
    i' = ord i - ord 'a'
    j' = ord j - ord 'a'


parse :: String -> Dance
parse = foldMap parseMove . splitOn ","
  where
    parseMove :: String -> Dance
    parseMove = \case
      's':(read->n)            -> spin n
      'x':(splitOn "/"->n:m:_) -> exch (read n) (read m)
      'p':n:_:m:_              -> part n m
      _                        -> error "No parse"

initial :: V.Vector Char
initial = V.fromList ['a' .. 'p']

day16a = undefined
day16b = undefined

-- day16a :: Challenge
-- day16a = V.toList
--        . foldl' step initial
--        . parse

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
