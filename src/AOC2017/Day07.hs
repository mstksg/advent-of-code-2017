{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}

module AOC2017.Day07 (day07a, day07b) where

import           AOC2017.Types       (Challenge)
import           Control.Applicative ((<|>))
import           Data.Char           (isAlpha)
import           Data.Foldable       (toList)
import           Data.List           (sortOn)
import           Data.Maybe          (mapMaybe, listToMaybe, fromJust)
import           Data.Tree
import qualified Data.Map            as M
import qualified Data.Set            as S

parseLine :: String -> (String, (Int, S.Set String))
parseLine (words->p:w:ws) =
    (p, (read w, S.fromList (filter isAlpha <$> drop 1 ws)))
parseLine _ = error "No parse"

-- | Returns the root label and the tree
buildTree
    :: M.Map String (Int, S.Set String)
    -> (String, Tree Int)
buildTree m = (root, result)
  where
    allChildren :: S.Set String
    allChildren = S.unions (snd <$> toList m)
    root :: String
    root = S.findMax $ M.keysSet m `S.difference` allChildren

    result :: Tree Int
    result = flip unfoldTree root $ \p ->
      let (w, cs) = m M.! p
      in  (w, toList cs)

-- | Check if any children are bad; otherwise, check yourself
findBad :: Tree Int -> Maybe Int
findBad t0 = listToMaybe badChildren <|> anomaly
  where
    badChildren :: [Int]
    badChildren = mapMaybe findBad $ subForest t0
    weightMap :: M.Map Int [Int]
    weightMap = M.fromListWith (++)
              . map (\t -> (sum t, [rootLabel t]))
              $ subForest t0
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

parse :: String -> (String, Tree Int)
parse = buildTree . M.fromList . map parseLine . lines

day07a :: Challenge
day07a = fst . parse

day07b :: Challenge
day07b = show . fromJust . findBad . snd . parse
