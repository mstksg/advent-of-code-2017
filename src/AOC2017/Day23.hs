module AOC2017.Day23 (day23a, day23b) where

import           AOC2017.Types                    (Challenge)
import           AOC2017.Util.Tape                (Tape(..), HasTape(..), unsafeTape, move)
import           Control.Applicative              (many)
import           Control.Lens                     (makePrisms, makeClassy, use, non, at, (.=), (%=), forMOf_)
import           Control.Monad.State              (StateT(..), lift)
import           Control.Monad.Trans.Maybe        (MaybeT(..))
import           Control.Monad.Writer             (Writer, runWriter, tell, Sum(..))
import           Data.Char                        (isAlpha)
import           Math.NumberTheory.Primes.Testing (isPrime)
import qualified Data.Map                         as M

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
day23b _ = show . length
         . filter (not . isPrime)
         $ [109900, 109917 .. 126900]
