{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}

module AOC2017.Day07 (day07a, day07b) where

import           AOC2017.Types       (Challenge)
import           Control.Applicative
import           Data.Char
import           Data.Foldable
import           Data.List
import           Data.Maybe (mapMaybe, listToMaybe, fromJust)
import qualified Data.Map            as M
import qualified Data.Set            as S

data Tree = Tree { _tParent :: String
                 , _tWeight :: Int
                 , _tLeaves :: [Tree]
                 }
          deriving Show

totalWeight :: Tree -> Int
totalWeight (Tree _ w ts) = w + sum (totalWeight <$> ts)

parseLine :: String -> (String, (Int, S.Set String))
parseLine (words->p:w:ws) =
    (p, (read w, S.fromList (filter isAlpha <$> drop 1 ws)))
parseLine _ = error "No parse"

findRoot :: M.Map String (a, S.Set String) -> String
findRoot m = S.findMax $ M.keysSet m `S.difference` allChildren
  where
    allChildren = S.unions (snd <$> toList m)

buildTree :: M.Map String (Int, S.Set String) -> Tree
buildTree m = go (findRoot m)
  where
    go :: String -> Tree
    go p = Tree p w (go <$> S.toList cs)
      where
        (w, cs) = m M.! p

-- | Check if any children are bad; otherwise, check yourself
findBad :: Tree -> Maybe Int
findBad (Tree _ _ ts) = listToMaybe badChildren <|> anomaly
  where
    badChildren = mapMaybe findBad ts
    weightMap = M.fromListWith (++)
              . map (\t -> (totalWeight t, [_tWeight t]))
              . toList
              $ ts
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
