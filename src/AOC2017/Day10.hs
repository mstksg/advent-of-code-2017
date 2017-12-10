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

swap :: Int -> Int -> V.Vector Int -> V.Vector Int
swap n s v = v V.// zip ixes (reverse ((v V.!) <$> ixes))
  where
    ixes = (`mod` V.length v) <$> [ n .. (n + s) - 1 ]


day10a :: Challenge
day10a  = show
        . product
        . take 2
        . V.toList
        . (\(x,y,z) -> x)
        . (\v' -> foldl' go (V.generate 256 id, 0, 0) v')
        . map read
        . words
        . map (\case ',' -> ' '; x -> x)
  where
    go  :: (V.Vector Int, Int, Int)
        -> Int
        -> (V.Vector Int, Int, Int)
    go (v0, i0, s0) amt
        = (swap i0 amt v0, (i0 + amt + s0) `mod` V.length v0, s0 + 1)

thing :: [Int] -> V.Vector Int
thing xs = v'
  where
    (v',_, _) = foldl' go (V.generate 256 id, 0, 0) xs
    go  :: (V.Vector Int, Int, Int)
        -> Int
        -> (V.Vector Int, Int, Int)
    go (v0, i0, s0) amt
        = (swap i0 amt v0, (i0 + amt + s0) `mod` V.length v0, s0 + 1)

day10b :: Challenge
day10b = concat . map (printf "%02x" . hash) . chunksOf 16 . V.toList . thing . concat . (replicate 64) . (++ [17, 31, 73, 47, 23]) . map ord . T.unpack . T.strip . T.pack
  where
    hash :: [Int] -> Int
    hash = fromIntegral @Word8 . foldr xor 0 . map fromIntegral

-- 74adb99c18b890f1fc47e6ed5b7b6dc
-- (3efbe78a8d82f2997931a4aa0b16a9d)
-- (3efbe78a8d82f29979031a4aa0b16a9d)
-- [
--
-- 74adb99c18b8900f1fc47e6ed5b7b6dc
-- 74adb99c18b8900f1fc47e6ed5b7b6dc
-- 74adb99c18b890f1fc47e6ed5b7b6dc
