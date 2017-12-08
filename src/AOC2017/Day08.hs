{-# LANGUAGE ViewPatterns #-}

module AOC2017.Day08 (day08a, day08b) where

import           AOC2017.Types  (Challenge)
import           Data.List      (foldl', scanl')
import           Data.Semigroup (Max(..))
import qualified Data.Map       as M

data Instr = Instr { _iRegister  :: String
                   , _iUpdate    :: Int
                   , _iCondReg   :: String
                   , _iPredicate :: Int -> Bool
                   }

step :: M.Map String Int -> Instr -> M.Map String Int
step m (Instr r u c p)
  | p (M.findWithDefault 0 c m) = M.insertWith (+) r u m
  | otherwise                   = m

parseLine :: String -> Instr
parseLine (words->r:f:u:_:c:o:x:_) =
    Instr { _iRegister  = r
          , _iUpdate    = f' (read u)
          , _iCondReg   = c
          , _iPredicate = (`op` read x)
          }
  where
    f' = case f of
      "dec" -> negate
      _     -> id
    op = case o of
      ">"  -> (>)
      ">=" -> (>=)
      "<"  -> (<)
      "<=" -> (<=)
      "==" -> (==)
      "!=" -> (/=)
      _    -> error "Invalid op"
parseLine _ = error "No parse"

parse :: String -> [Instr]
parse = map parseLine . lines

day08a :: Challenge
day08a = show . maximum
       . foldl' step M.empty
       . parse

day08b :: Challenge
day08b = show . getMax . (foldMap . foldMap) Max
       . scanl' step M.empty
       . parse
