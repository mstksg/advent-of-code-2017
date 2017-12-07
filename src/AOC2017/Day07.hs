{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}

module AOC2017.Day07 (day07a, day07b) where

import           AOC2017.Types       (Challenge)
import           Control.Applicative ((<|>))
import           Data.Char           (isAlpha)
import           Data.Foldable       (toList)
import           Data.List           (sortOn)
import           Data.Maybe          (mapMaybe, listToMaybe, fromJust)
import qualified Data.Map            as M
import qualified Data.Set            as S

data Tree = Tree { _tParent :: String
                 , _tWeight :: Int
                 , _tLeaves :: [Tree]
                 }
          deriving Show

type Report = M.Map String (Int, S.Set String)

totalWeight :: Tree -> Int
totalWeight (Tree _ w ts) = w + sum (totalWeight <$> ts)

parseLine :: String -> (String, (Int, S.Set String))
parseLine (words->p:w:ws) =
    (p, (read w, S.fromList (filter isAlpha <$> drop 1 ws)))
parseLine _ = error "No parse"

buildTree :: Report -> Tree
buildTree m = go root
  where
    allChildren :: S.Set String
    allChildren = S.unions (snd <$> toList m)
    root :: String
    root = S.findMax $ M.keysSet m `S.difference` allChildren
    go :: String -> Tree
    go p = Tree p w (go <$> S.toList cs)
      where
        (w, cs) = m M.! p

-- | Check if any children are bad; otherwise, check yourself
findBad :: Tree -> Maybe Int
findBad (Tree _ _ ts) = listToMaybe badChildren <|> anomaly
  where
    badChildren :: [Int]
    badChildren = mapMaybe findBad ts
    weightMap :: M.Map Int [Int]
    weightMap = M.fromListWith (++)
              . map (\t -> (totalWeight t, [_tWeight t]))
              . toList
              $ ts
    anomaly :: Maybe Int
    anomaly = case sortOn (length . snd) (M.toList weightMap) of
      -- end of the line
      []                       -> Nothing
      -- all weights match
      [_]                      -> Nothing
      -- exactly one anomaly
      [(wTot1, [w]),(wTot2,_)] -> Just (w + (wTot2 - wTot1))
      -- should not happen
      _                        -> error "More than one anomaly for node"

parse :: String -> Tree
parse = buildTree . M.fromList . map parseLine . lines

day07a :: Challenge
day07a = _tParent . parse

day07b :: Challenge
day07b = show . fromJust . findBad . parse
