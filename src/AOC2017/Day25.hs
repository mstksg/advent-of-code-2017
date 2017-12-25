module AOC2017.Day25 (day25a) where

import           AOC2017.Types              (Challenge)
import           AOC2017.Util               ((!!!))
import           Control.Applicative        (Applicative(..), Alternative(..))
import           Control.Lens.At            (Contains(..))
import           Control.Monad              (replicateM)
import           Data.Bifunctor             (first)
import           Data.Semigroup             (Semigroup(..), Last(..), Sum(..))
import           Data.Void                  (Void)
import qualified Data.IntSet                as IS
import qualified Data.Map                   as M
import qualified Text.Megaparsec            as P
import qualified Text.Megaparsec.Char       as P
import qualified Text.Megaparsec.Char.Lexer as PL

type St      = Last Char
type Step    = (Sum Int, St)
type RuleMap = M.Map St ((Step, Bool), (Step, Bool))

runRule :: RuleMap -> St -> Bool -> (Step, Bool)
runRule rm st = \case
    False -> fst $ rm M.! st
    True  -> snd $ rm M.! st

step :: RuleMap -> Step -> IS.IntSet -> (Step, IS.IntSet)
step rm s0@(Sum i,st) = first (s0 <>) . contains i (runRule rm st)

day25a :: Challenge
day25a (parse->RS{..}) = show . IS.size . snd . (!!! rsSteps)
                       . iterate (uncurry (step rsRuleMap))
                       $ ((0, rsStart), IS.empty)



-- Ugly parser stuff below

data RuleSet = RS { rsStart   :: St
                  , rsSteps   :: Int
                  , rsRuleMap :: RuleMap
                  }
  deriving Show

type Parser = P.Parsec Void String

parse :: String -> RuleSet
parse = either (error . P.parseErrorPretty) id . P.runParser ruleSet ""

ruleSet :: Parser RuleSet
ruleSet = do
    st0 <- parseLine "Begin in state"                      P.anyChar
    stp <- parseLine "Perform a diagnostic checksum after" PL.decimal
    rules <- many rule
    return RS { rsStart   = Last st0
              , rsSteps   = stp
              , rsRuleMap = M.fromList rules
              }
  where
    rule :: Parser (Last Char, ((Step, Bool), (Step, Bool)))
    rule = do
      st <- parseLine "In state" P.anyChar
      [r0,r1] <- replicateM 2 $ do
        _ <- parseLine "If the current value is" P.anyChar
        b <- (== '1')
         <$> parseLine "- Write the value"       P.anyChar
        d <- (\case "left" -> -1; _ -> 1)
         <$> parseLine "- Move one slot to the"  (P.many P.letterChar)
        c <- parseLine "- Continue with state"   P.anyChar
        return ((Sum d, Last c), b)
      return (Last st, (r0, r1))
    parseLine :: String -> Parser a -> Parser a
    parseLine str = P.between (P.space *> P.string str *> P.space)
                              (P.skipManyTill P.anyChar P.newline)
