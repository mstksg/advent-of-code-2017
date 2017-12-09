{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}

module AOC2017.Day09 (day09a, day09b) where

import           AOC2017.Types              (Challenge)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Writer
import           Data.Either
import           Data.Monoid
import           Data.Void
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P

data Tree = Garbage
          | Group [Tree]

type Parser = P.ParsecT Void String (Writer (Sum Int))

parseTree :: Parser Tree
parseTree = P.choice [ Group   <$> parseGroup
                     , Garbage <$  parseGarbage
                     ]
  where
    parseGroup :: Parser [Tree]
    parseGroup = do
      _  <- P.char '{'
      ts <- parseTree `P.sepBy` P.char ','
      _  <- P.char '}'
      return ts
    parseGarbage :: Parser ()
    parseGarbage = P.char '<' *> inGarbage
      where
        inGarbage :: Parser ()
        inGarbage = P.choice
          [ () <$ P.char '>'
          , P.char '!' *> P.anyChar     *> inGarbage
          , P.anyChar  *> lift (tell 1) *> inGarbage
          ]

treeScore :: Tree -> Int
treeScore = go 1
  where
    go n = \case
      Garbage  -> 0
      Group ts -> n + sum (go (n + 1) <$> ts)

day09a :: Challenge
day09a = show . treeScore
       . fromRight undefined . fst . runWriter
       . P.runParserT parseTree ""

day09b :: Challenge
day09b = show . getSum
       . execWriter
       . P.runParserT parseTree ""

