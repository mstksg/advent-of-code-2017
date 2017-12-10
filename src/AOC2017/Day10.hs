{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns     #-}

module AOC2017.Day10 (day10a, day10b) where

import           AOC2017.Types   (Challenge)
import           Control.Monad
import           Data.Bits
import           Data.Char
import           Data.List
import           Data.List.Split
import           Data.Word
import           Debug.Trace
import           Text.Printf
import qualified Data.Text       as T
import qualified Data.Vector     as V

data HashState = HS { _hsVec  :: V.Vector Int
                    , _hsPos  :: Int
                    , _hsSkip :: Int
                    }

initHS :: HashState
initHS = HS (V.generate 256 id) 0 0

step :: HashState -> Int -> HashState
step (HS v0 p0 s0) n = HS v1 p1 s1
  where
    ixes = (`mod` V.length v0) <$> [ p0 .. p0 + n - 1 ]
    v1 = v0 V.// zip ixes ((v0 V.!) <$> reverse ixes)
    p1 = (p0 + n + s0) `mod` V.length v0
    s1 = s0 + 1

day10a :: Challenge
day10a = show . product . V.take 2 . _hsVec
       . foldl' step initHS
       . map read . splitOn ","

day10b :: Challenge
day10b = concatMap (printf "%02x" . foldr xor 0)
       . chunksOf 16 . V.toList . _hsVec
       . foldl' step initHS
       . concat . replicate 64 . (++ salt) . map ord
       . T.unpack . T.strip . T.pack
  where
    salt = [17, 31, 73, 47, 23]

