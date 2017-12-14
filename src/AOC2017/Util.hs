module AOC2017.Util (
    strip
  ) where

import qualified Data.Text as T

strip :: String -> String
strip = T.unpack . T.strip . T.pack

