{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module AOC2017.Day10 (day10a, day10b) where

import           AOC2017.Types              (Challenge)
import           Data.Bits                  (xor)
import           Data.Char                  (ord)
import           Data.Finite                (Finite, modClass)
import           Data.List                  (foldl')
import           Data.List.Split            (chunksOf, splitOn)
import           GHC.TypeNats               (KnownNat)
import           Text.Printf                (printf)
import qualified Data.Text                  as T
import qualified Data.Vector.Storable.Sized as V

data HashState n = HS { _hsVec  :: V.Vector n Int
                      , _hsPos  :: Finite n
                      , _hsSkip :: Int
                      }

step :: KnownNat n => HashState n -> Int -> HashState n
step (HS v0 p0 s0) n = HS v1 p1 s1
  where
    ixes = take n $ iterate (+ 1) p0
    vals = V.index v0 <$> ixes
    v1   = v0 V.// zip ixes (reverse vals)
    p1   = p0 + modClass (n + s0)
    s1   = s0 + 1

process :: KnownNat n => [Int] -> V.Vector n Int
process = _hsVec . foldl' step hs0
  where
    hs0 = HS (V.generate fromIntegral) 0 0

day10a :: Challenge
day10a = show . product . take 2 . V.toList
       . process @256
       . map read . splitOn ","

day10b :: Challenge
day10b = toHex . process @256
       . concat . replicate 64 . (++ salt)
       . map ord . strip
  where
    salt  = [17, 31, 73, 47, 23]
    toHex = concatMap (printf "%02x" . foldr xor 0) . chunksOf 16 . V.toList
    strip = T.unpack . T.strip . T.pack

