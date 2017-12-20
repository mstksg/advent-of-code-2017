module AOC2017.Util (
    strip
  , iterateMaybe
  , (!!!)
  , dup
  ) where

import           Data.List
import qualified Data.Text         as T

-- | Strict (!!)
(!!!) :: [a] -> Int -> a
[] !!! _ = error "Out of range"
(x:_ ) !!! 0 = x
(x:xs) !!! n = x `seq` (xs !!! (n - 1))

strip :: String -> String
strip = T.unpack . T.strip . T.pack

iterateMaybe :: (a -> Maybe a) -> a -> [a]
iterateMaybe f x0 = x0 : unfoldr (fmap dup . f) x0

dup :: a -> (a, a)
dup x = (x, x)
