{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module AOC2017.Day09 (day09a, day09b) where

import           AOC2017.Types        (Challenge)
import           Data.Functor         (($>))
import           Data.Void            (Void)
import qualified Text.Megaparsec      as P
import qualified Text.Megaparsec.Char as P

data Tree = Garbage Int
          | Group [Tree]

type Parser = P.Parsec Void String

parseTree :: Parser Tree
parseTree = P.choice [ Group   <$> parseGroup
                     , Garbage <$> parseGarbage
                     ]
  where
    parseGroup :: Parser [Tree]
    parseGroup = P.between (P.char '{') (P.char '}') $
       parseTree `P.sepBy` P.char ','
    parseGarbage :: Parser Int
    parseGarbage = P.char '<' *> eatGarbage
      where
        eatGarbage :: Parser Int
        eatGarbage = P.choice
          [ P.char '>' $> 0
          , P.char '!' *> P.anyChar   *> eatGarbage
          , P.anyChar  *>      (succ <$> eatGarbage)
          ]

treeScore :: Tree -> Int
treeScore _ (Garbage _ ) = 0
treeScore n (Group   ts) = n + sum (treeScore (n + 1) <$> ts)

treeGarbage :: Tree -> Int
treeGarbage (Garbage n ) = n
treeGarbage (Group   ts) = sum (treeGarbage <$> ts)

parse :: String -> Tree
parse = either (error . show) id . P.runParser parseTree ""

day09a :: Challenge
day09a = show . treeScore   . parse

day09b :: Challenge
day09b = show . treeGarbage . parse
