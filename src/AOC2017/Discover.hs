
module AOC2017.Discover (
  ) where

import           AOC2017.Types
import           Data.IntMap            (IntMap)
import           Data.Map               (Map)
import           Language.Haskell.Exts
import           Language.Haskell.Names
import qualified Data.IntMap            as IM
import qualified Data.Map               as M

type ChallengeMap = IntMap (Map Char Challenge)

