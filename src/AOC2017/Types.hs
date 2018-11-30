module AOC2017.Types (
    Challenge
  , C(..), runC
  , ChallengeMap
  , ChallengeSpec(..)
  ) where

import           Data.Finite
import           Data.Kind
import           Data.Map    (Map)
import           Data.Maybe

type Challenge = String -> String

data C :: Type where
    C :: { cParse :: String  -> Maybe a
         , cSolve :: a       -> Maybe b
         , cShow  :: b       -> String
         }
      -> C

runC :: C -> Challenge
runC C{..} = cShow . fromJust . cSolve . fromJust . cParse

type ChallengeMap = Map (Finite 25) (Map Char Challenge)

data ChallengeSpec = CS { _csDay  :: Finite 25
                        , _csPart :: Char
                        }
