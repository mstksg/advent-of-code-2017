module AOC2017.Challenge.Day23 (day23a, day23b) where

import           AOC2017.Types
import           Control.Applicative              (many)
import           Control.Lens                     (set, use, at, non, _last, forMOf_, Iso', iso)
import           Control.Lens.Operators           ((.=), (%=))
import           Control.Lens.TH                  (makeClassy, makePrisms)
import           Control.Lens.Tuple               (_1, _2, _3)
import           Control.Monad.State              (StateT(..), lift)
import           Control.Monad.Trans.Maybe        (MaybeT(..))
import           Control.Monad.Writer             (Writer, runWriter, tell, Sum(..))
import           Data.Char                        (isAlpha)
import           Math.NumberTheory.Primes.Testing (isPrime)
import qualified Data.List.PointedList            as PL
import qualified Data.Map                         as M

type Addr = Either Char Int

addr :: String -> Addr
addr [c] | isAlpha c = Left c
addr str = Right (read str)

data BinOp = BOSet | BOSub | BOMul
makePrisms ''BinOp
runBO :: BinOp -> Int -> Int -> Int
runBO = \case { BOSet -> const id; BOSub -> (-); BOMul -> (*) }

data JumpCond = JCNotZero | JCPrime
runJC :: JumpCond -> Int -> Bool
runJC = \case { JCNotZero -> (/= 0); JCPrime -> isPrime . fromIntegral }

data Op = OBin BinOp Char Addr
        | OJmp JumpCond Addr Int
makePrisms ''Op

parseOp :: String -> Op
parseOp inp = case words inp of
    "set":(x:_):(addr->y):_     -> OBin BOSet x y
    "sub":(x:_):(addr->y):_     -> OBin BOSub x y
    "mul":(x:_):(addr->y):_     -> OBin BOMul x y
    "jnz":(addr->x):(read->y):_ -> OJmp JCNotZero x y
    "jpm":(addr->x):(read->y):_ -> OJmp JCPrime x y
    _                           -> error "Bad parse"

parse :: String -> [Op]
parse = map parseOp . lines

-- | Replaces the two inner loops with a simple prime check
optimize :: [Op] -> [Op]
optimize = set (     _last . _OJmp . _3     ) (-7)
         . set (splot 8 . _2 . splot 17 . _1) [parseOp "jpm b 2"]
  where
    -- split by prefix and suffx
    splot :: Int -> Iso' [a] ([a], [a])
    splot n = iso (splitAt n) (uncurry (++))

data ProgState = PS { _psTape :: PL.PointedList Op
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
stepTape = use (psTape . PL.focus) >>= \case
    OBin bo x y -> do
      yVal <- addrVal y
      psRegs . at x . non 0 %= \xVal -> runBO bo xVal yVal
      forMOf_ _BOMul bo $ \_ ->
        lift . lift $ tell (Sum 1)
      advance 1
    OJmp jc x y -> do
      xVal <- addrVal x
      let moveAmt | runJC jc xVal = y
                  | otherwise     = 1
      advance moveAmt
  where
    addrVal (Left r)  = use (psRegs . at r . non 0)
    addrVal (Right x) = return x
    advance n = do
      Just t' <- PL.moveN n <$> use psTape
      psTape .= t'

day23a :: Challenge
day23a = runC C
    { cParse = PL.fromList . parse
    , cShow  = show . getSum
    , cSolve = Just . snd
             . runTapeProg (many stepTape)    -- stepTape until program terminates
             . (`PS` M.empty)
    }

day23b :: Challenge
day23b = runC C
    { cParse = PL.fromList . optimize . parse
    , cShow  = show
    , cSolve = Just . M.findWithDefault 0 'h' . _psRegs . snd . fst
             . runTapeProg (many stepTape)
             . (`PS` M.singleton 'a' 1)
    }
