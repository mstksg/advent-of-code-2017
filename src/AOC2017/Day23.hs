{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- module AOC2017.Day23 (day23a, day23b) where
module AOC2017.Day23 where

import           AOC2017.Types                    (Challenge)
import           AOC2017.Util
import           AOC2017.Util.Tape
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Prompt
import           Control.Monad.State
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Writer
import           Data.Bifunctor
import           Data.Char
import           Data.Foldable
import           Data.Kind
import           Data.Maybe
import           Data.Tuple
import           Debug.Trace
import           Math.NumberTheory.Primes.Testing (isPrime)
import           Numeric.Natural
import           Text.Pretty.Simple
import           Text.Read                        (readMaybe)
import qualified Data.IntMap                      as IM
import qualified Data.IntSet                      as IS
import qualified Data.Map                         as M
import qualified Data.Text                        as T
import qualified Data.Text.Lazy                   as TL

type Addr = Either Char Int

addr :: String -> Addr
addr [c] | isAlpha c = Left c
addr str = Right (read str)

data BinOp = BOSet | BOSub | BOMul
  deriving (Show, Eq)
makePrisms ''BinOp

runBO :: BinOp -> Int -> Int -> Int
runBO BOSet = const id
runBO BOSub = (-)
runBO BOMul = (*)

data Op = OBin BinOp Char Addr
        | OJnz Addr Int
  deriving Show

parseOp :: String -> Op
parseOp inp = case words inp of
    "set":(x:_):(addr->y):_     -> OBin BOSet x y
    "sub":(x:_):(addr->y):_     -> OBin BOSub x y
    "mul":(x:_):(addr->y):_     -> OBin BOMul x y
    "jnz":(addr->x):(read->y):_ -> OJnz x y
    _                           -> error "Bad parse"

parse :: String -> Tape Op
parse = unsafeTape . map parseOp . lines

data ProgState = PS { _psTape :: Tape Op
                    , _psRegs :: M.Map Char Int
                    }

makeClassy ''ProgState

-- | Context in which Tape commands are run.  Writer parameter records
-- number of Mul's
--
-- Nothing = program terminates by running out of bounds
type TapeProg = MaybeT (StateT ProgState (Writer (Sum Int)))
runTapeProg :: TapeProg a -> ProgState -> ((Maybe a, ProgState), Sum Int)
runTapeProg tp ps = runWriter . flip runStateT ps . runMaybeT $ tp

-- | Single step through program tape.
stepTape :: TapeProg ()
stepTape = use (psTape . tFocus) >>= \case
    OBin bo x y -> do
      yVal <- addrVal y
      psRegs . at x . non 0 %= \xVal -> runBO bo xVal yVal
      forMOf_ _BOMul bo $ \_ ->
        lift . lift $ tell (Sum 1)
      advance 1
    OJnz x y -> do
      xVal <- addrVal x
      let moveAmt | xVal /= 0 = y
                  | otherwise = 1
      advance moveAmt
  where
    addrVal (Left r)  = use (psRegs . at r . non 0)
    addrVal (Right x) = return x
    advance n = do
      Just t' <- move n <$> use psTape
      psTape .= t'

day23a :: Challenge
day23a = show . getSum . snd
       . runTapeProg (many stepTape)    -- stepTape until program terminates
       . (`PS` M.empty) . parse

-- | hardcoded for now
day23b :: Challenge
day23b = undefined

data P1 = P1Bin BinOp Char Addr
        | P1Jump Addr Natural
        | P1While Addr (IM.IntMap P1)
  deriving Show
makePrisms ''P1

pass1 :: IM.IntMap Op -> IM.IntMap P1
pass1 opMap = case IM.minViewWithKey whileMap of
    Nothing -> IM.union (review _P1Bin  <$> noJumps)
                        (review _P1Jump <$> ifs    )
    Just ((lStart, lEnds), _) -> case IM.splitLookup lStart opMap of
      (befJump, fromJust->jumpTarg, aftJump)
        | IM.null befJump -> -- loop starts here
            let (loopEnd, loopCond) = IM.findMax lEnds
            in  case IM.splitLookup loopEnd (IM.insert lStart jumpTarg aftJump) of
                  (inWhile, fromJust-> _, aftWhile) ->
                    IM.insert lStart (P1While loopCond (pass1 inWhile))
                        $ pass1 aftWhile
        | otherwise -> -- stuff before loop
            IM.union (pass1 befJump) (pass1 (IM.insert lStart jumpTarg aftJump))
  where
    (jumps, noJumps) = flip IM.mapEither opMap $ \case
        OBin bo r x -> Right (bo, r, x)
        OJnz r j    -> Left (r, j)
    (whiles, ifs) = flip IM.mapEither jumps $ \(r,j) ->
                      bimap (r,) (r,) (splitNat j)
    whileMap = IM.fromListWith IM.union
             . map (\(i,(r, j)) -> (i - fromIntegral j, IM.singleton i r))
             $ IM.toList whiles

splitNat :: Integral a => a -> Either Natural Natural
splitNat n = case compare n 0 of
    LT -> Left (fromIntegral (abs n))
    EQ -> Right (fromIntegral n)
    GT -> Right (fromIntegral n)

lookupIMMin :: IM.IntMap a -> Maybe (Int, a)
lookupIMMin m | IM.null m = Nothing
              | otherwise = Just $ IM.findMin m

lookupIMMax :: IM.IntMap a -> Maybe (Int, a)
lookupIMMax m | IM.null m = Nothing
              | otherwise = Just $ IM.findMax m
