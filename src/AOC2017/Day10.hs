{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

module AOC2017.Day10 (day10a, day10b) where

import           AOC2017.Types     (Challenge)
import           Data.Bits         (xor)
import           Data.Char         (ord)
import           Data.Finite       (Finite, finite, modClass)
import           Data.Foldable     (toList)
import           Data.List         (foldl')
import           Data.List.Split   (chunksOf, splitOn)
import           GHC.TypeNats      (KnownNat)
import           Text.Printf       (printf)
import qualified Data.Text         as T
import qualified Data.Vector.Sized as V

data HashState n = HS { _hsVec  :: V.Vector n Int
                      , _hsPos  :: Finite n
                      , _hsSkip :: Integer
                      }

initHS :: KnownNat n => HashState n
initHS = HS (V.generate fromIntegral) 0 0

step :: KnownNat n => HashState n -> Finite n -> HashState n
step (HS v0 p0 s0) n = HS v1 p1 s1
  where
    ixes = take (fromIntegral n) $ iterate (+ 1) p0
    v1   = v0 V.// zip ixes (V.index v0 <$> reverse ixes)
    p1   = p0 + n + modClass s0
    s1   = s0 + 1

day10a :: Challenge
day10a = show . product . take 2 . toList . _hsVec
       . foldl' step (initHS @256)
       . map (finite . read @Integer) . splitOn ","

day10b :: Challenge
day10b = concatMap (printf "%02x" . foldr xor 0)
       . chunksOf 16 . V.toList . _hsVec
       . foldl' step (initHS @256)
       . concat . replicate 64 . (++ salt) . map (fromIntegral . ord)
       . T.unpack . T.strip . T.pack
  where
    salt = [17, 31, 73, 47, 23]

