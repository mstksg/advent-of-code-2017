{-# LANGUAGE BangPatterns #-}

module AOC2017.Day16 (day16a, day16b) where

import           AOC2017.Types   (Challenge)
import           Data.List
import           Data.List.Split
import qualified Data.Vector     as V

data Move = MSpin     Int
          | MExchange Int  Int
          | MPartner  Char Char

step :: V.Vector Char -> Move -> V.Vector Char
step v0 = \case
  MSpin n       -> let (x,y) = V.splitAt (16 - n) v0
                   in  y V.++ x
  MExchange n m -> v0 V.// [(n, v0 V.! m),(m, v0 V.! n)]
  MPartner n m  -> let Just iN = V.findIndex (== n) v0
                       Just iM = V.findIndex (== m) v0
                   in  v0 V.// [(iN, m),(iM, n)]

parse :: String -> [Move]
parse = map parseMove . splitOn ","
  where
    parseMove :: String -> Move
    parseMove = \case
      's':(read->n)            -> MSpin n
      'x':(splitOn "/"->n:m:_) -> MExchange (read n) (read m)
      'p':n:_:m:_              -> MPartner n m
      _                        -> error "No parse"

initial :: V.Vector Char
initial = V.fromList ['a' .. 'p']

day16a :: Challenge
day16a = V.toList
       . foldl' step initial
       . parse

day16b :: Challenge
day16b (parse->moves) = V.toList $
    iterate oneCycle initial !! leftovers
  where
    oneCycle xs = foldl' step xs moves
    loopLength = (+ 1)
               . length
               . takeWhile (/= initial)
               . drop 1
               . iterate oneCycle
               $ initial
    leftovers = 1000000000 `mod` loopLength
