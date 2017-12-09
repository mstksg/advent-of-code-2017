{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module AOC2017.Day09 (day09a, day09b) where

import           AOC2017.Types        (Challenge)
import           Control.Applicative
import           Data.Either
import           Data.Tree
import           Data.Void
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P

type Parser = P.Parsec Void String

-- THIS IS JUST WRITERRRRRR
parse :: Int -> Parser (Tree Int, Int)
parse n = P.try parseTree <|> ((Node 0 [],) <$> parseGarbage)
  where
    parseTree :: Parser (Tree Int, Int)
    parseTree = do
      P.char '{'
      (ts,ns) <- unzip <$> parse (n + 1) `P.sepBy` P.try (P.char ',')
      P.char '}'
      return (Node n ts, sum ns)
    parseGarbage :: Parser Int
    parseGarbage = do
      P.char '<'
      inGarbage 0
    inGarbage :: Int -> Parser Int
    inGarbage n = do
      c <- P.anyChar
      case c of
        '>' -> return n
        '!' -> P.anyChar *> inGarbage n
        _   -> inGarbage (n + 1)

treeScore :: Tree Int -> Int
treeScore = foldTree $ \i ns -> i + sum ns

day09a :: Challenge
day09a = either show (show . treeScore . fst) . P.runParser (parse 1) ""
-- day09a = either show (('\n':) . drawTree . fmap show) . P.runParser (parse 1) ""

day09b :: Challenge
day09b = either show (show . snd) . P.runParser (parse 1) ""

-- process
--     :: [Char]
--     -> Int
-- process = go 0 False
--   where
--     go :: Int -> Bool -> [Char] -> Int
--     go d True = \case
--       '!':_:xs -> go d True xs
--       '>':xs   -> go d False xs
--       _:xs     -> go d True xs
--     go d False = \case
--       '<':xs -> go d False xs
--       '{':xs -> d + go (d + 1) False xs
--       '}':xs -> go (d - 1) False xs


-- process
--     :: [Char]
--     -> Tree ()
-- process = Tree () (go False [])
--   where
--     go :: Bool -> [Tree ()] -> [Char] -> [Tree ()]
--     go True ts = \case
--       '!':_:xs -> go True ts xs
--       '>':xs   -> go False ts xs
--       _:xs     -> go True ts xs
--     go False ts = \case
--       '{':xs -> go False


