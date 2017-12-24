module AOC2017.Day24 (day24a, day24b) where

import           AOC2017.Types             (Challenge)
import           Control.Applicative       (Alternative(..))
import           Control.Monad.Trans.State (StateT(..), evalStateT)
import           Data.List                 (maximumBy)
import           Data.List.Split           (splitOn)
import           Data.Monoid               ((<>))
import           Data.Ord                  (comparing)

type Comp = (Int, Int)

-- | All possible ways of selecting a single item from a list
select :: [a] -> [(a, [a])]
select []     = []
select (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- select xs]

bridge :: Int -> StateT [Comp] [] Int
bridge frm = do
    (x,y) <- StateT select
    next <- if | x == frm  -> return y
               | y == frm  -> return x
               | otherwise -> empty
    rest <- return 0        -- account for a bridge that ends here
        <|> bridge next     -- account for a continued bridge
    return $ x + y + rest

day24a :: Challenge
day24a = show . maximum . evalStateT (bridge 0) . parse

day24b :: Challenge
day24b = show . fst
       . maximumBy ((<>) <$> comparing (negate . length . snd)
                         <*> comparing fst
                   )
       . runStateT (bridge 0) . parse

parse :: String -> [Comp]
parse = map parseLine . lines
  where
    parseLine (splitOn "/"->(x:y:_)) = (read x, read y)
    parseLine _ = error "No parse"

