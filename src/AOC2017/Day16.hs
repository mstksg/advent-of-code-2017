module AOC2017.Day16 (day16a, day16b) where

-- import qualified Numeric.LinearAlgebra.Static as H
import           AOC2017.Types                   (Challenge)
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

permVec :: H.Vector H.I -> H.Matrix Double
permVec v = H.ident 16 H.?? (H.PosCyc v, H.All)

spin :: Int -> Dance
spin n = mempty { _dPosPerm = permVec $
                      16 H.|> drop (n - 1) (cycle [0..15])
                }

swap :: Int -> Int -> Dance
swap i j = mempty { _dPosPerm = permVec $ H.assoc 16 0
                        [ (fromIntegral k, fromIntegral x) | k <- [0..15]
                                              , let x | x == i = j
                                                      | x == j = i
                                                      | otherwise = k
                        ]
                  }

-- -- spin (someNatVal->SomeNat (Proxy :: Proxy n)) =
-- --       let (top, bottom) = H.splitRows @n @16 @16 H.eye
-- --       in  D (bottom H.=== top) H.eye

-- partner :: Char -> Char -> Dance
-- partner (charIx->x) (charIx->y) = D H.eye . H.build $ \i j ->

-- charIx :: Char -> Int
-- charIx = subtract (ord 'a') . ord

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

-- parse :: String -> DancePerm
-- parse = foldMap parseMove . splitOn ","
--   where
--     parseMove :: String -> DancePerm
--     parseMove = \case
--       's':(read->n)            -> MSpin n
--       'x':(splitOn "/"->n:m:_) -> MExchange (read n) (read m)
--       'p':n:_:m:_              -> MPartner n m
--       _                        -> error "No parse"

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
