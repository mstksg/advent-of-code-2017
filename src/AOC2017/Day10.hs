module AOC2017.Day10 (day10a, day10b, knothash) where

import           AOC2017.Types        (Challenge)
import           AOC2017.Util         (strip)
import           Data.Bits            (xor)
import           Data.Char            (ord)
import           Data.List            (foldl')
import           Data.List.Split      (chunksOf, splitOn)
import           Data.Word            (Word8)
import           Text.Printf          (printf)
import qualified Data.Vector.Storable as V

data HashState = HS { _hsVec  :: V.Vector Word8
                    , _hsPos  :: Word8
                    , _hsSkip :: Word8
                    }

step :: HashState -> Word8 -> HashState
step (HS v0 p0 s0) n = HS v1 p1 s1
  where
    ixes = fromIntegral . (+ p0) <$> init [0 .. n]
    vals = (v0 V.!) <$> ixes
    v1   = v0 V.// zip ixes (reverse vals)
    p1   = p0 + n + s0
    s1   = s0 + 1

process :: [Word8] -> V.Vector Word8
process = _hsVec . foldl' step hs0
  where
    hs0 = HS (V.generate 256 fromIntegral) 0 0

day10a :: Challenge
day10a = show . V.product . V.take 2
       . process
       . map read . splitOn ","

day10b :: Challenge
day10b = toHex . knothash . strip
  where
    toHex = concatMap (printf "%02x")

knothash :: String -> [Word8]
knothash = map (foldr xor 0) . chunksOf 16 . V.toList . process
         . concat . replicate 64 . (++ salt)
         . map (fromIntegral . ord)
  where
    salt  = [17, 31, 73, 47, 23]
