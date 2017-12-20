{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeInType             #-}

module AOC2017.Day18 (day18a, day18b) where

import           AOC2017.Types             (Challenge)
import           AOC2017.Util.Tape         (Tape(..), HasTape(..), move, unsafeTape)
import           Control.Applicative       (many, empty)
import           Control.Lens              (makeClassy, use, at, non, (%=), use, (.=), (<>=), zoom)
import           Control.Monad             (join, guard, when)
import           Control.Monad.Prompt      (Prompt, prompt, runPromptM)
import           Control.Monad.State       (StateT(..), execStateT, evalStateT, get, put)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.Monad.Writer      (Writer, runWriter, tell)
import           Data.Bifunctor
import           Data.Char                 (isAlpha)
import           Data.Kind                 (Type)
import           Data.Maybe                (fromJust, maybeToList)
import           Data.Tuple
import qualified Data.Map                  as M
import qualified Data.Vector.Sized         as V

type Addr = Either Char Int

addr :: String -> Addr
addr [c] | isAlpha c = Left c
addr str = Right (read str)

data Op = OSnd Addr
        | OBin (Int -> Int -> Int) Char Addr
        | ORcv Char
        | OJgz Addr Addr

instance Show Op where
    show _ = "Op"

parseOp :: String -> Op
parseOp inp = case words inp of
    "snd":(addr->c):_           -> OSnd c
    "set":(x:_):(addr->y):_     -> OBin (const id) x y
    "add":(x:_):(addr->y):_     -> OBin (+) x y
    "mul":(x:_):(addr->y):_     -> OBin (*) x y
    "mod":(x:_):(addr->y):_     -> OBin mod x y
    "rcv":(x:_):_               -> ORcv x
    "jgz":(addr->x):(addr->y):_ -> OJgz x y
    _                           -> error "Bad parse"

parse :: String -> Tape Op
parse = unsafeTape . map parseOp . lines

data ProgState = PS { _psTape :: Tape Op
                    , _psRegs :: M.Map Char Int
                    }

makeClassy ''ProgState

data Command :: Type -> Type where
    CRcv :: Int -> Command Int    -- ^ input is current value of buffer
    CSnd :: Int -> Command ()     -- ^ input is thing being sent

-- | Context in which Tape commands are run.  Tape commands have access to
-- an underlying 'Prompt Command' effect monad that allows it to 'Rcv' and
-- 'Snd'.
--
-- Nothing = program terminates by running out of bounds
type TapeProg = MaybeT (StateT ProgState (Prompt Command))
execTapeProg :: TapeProg a -> ProgState -> Prompt Command ProgState
execTapeProg tp ps = flip execStateT ps . runMaybeT $ tp

-- | Single step through program tape.
stepTape :: TapeProg ()
stepTape = use (psTape . tFocus) >>= \case
    OSnd x -> do
      lift . lift . prompt . CSnd =<< addrVal x
      advance 1
    OBin f x y -> do
      yVal <- addrVal y
      psRegs . at x . non 0 %= (`f` yVal)
      advance 1
    ORcv x -> do
      y <- lift . lift . prompt . CRcv
       =<< use (psRegs . at x . non 0)
      psRegs . at x .= Just y
      advance 1
    OJgz x y -> do
      xVal <- addrVal x
      moveAmt <- if xVal > 0
                   then addrVal y
                   else return 1
      advance moveAmt
  where
    addrVal (Left r)  = use (psRegs . at r . non 0)
    addrVal (Right x) = return x
    advance n = do
      Just t' <- move n <$> use psTape
      psTape .= t'

-- | Context in which to interpret Command for Part A
--
-- State parameter is the most recent sent item.  Writer parameter is all
-- of the Rcv'd items.
--
-- State should probably be Accum instead, but Accum is in any usable
-- version of transformers yet.
type PartA = StateT (Maybe Int) (Writer [Int])
execPartA :: PartA a -> Int
execPartA = head . snd . runWriter . flip execStateT Nothing

-- | Interpet Command for Part A
interpretA :: Command a -> PartA a
interpretA = \case
    CRcv x -> do
      when (x /= 0) $
        lift . tell . maybeToList =<< get
      return x
    CSnd x -> put (Just x)

day18a :: Challenge
day18a = show
       . execPartA . runPromptM interpretA
       . execTapeProg (many stepTape)    -- stepTape until program terminates
       . (`PS` M.empty) . parse

-- | Context in which to interpret Command for Part B
--
-- The State parameter is the input buffer, the Writer parameter is the
-- emitted items.  Maybe is whether or not the thread runs out of input
-- items
type PartB = MaybeT (StateT [Int] (Writer [Int]))
runPartB :: [Int] -> PartB a -> ((Maybe a, [Int]), [Int])
runPartB buf = runWriter . flip runStateT buf . runMaybeT

-- | Interpet Command for Part B
interpretB
    :: Command a
    -> PartB a
interpretB = \case
    CSnd x -> tell [x]
    CRcv _ -> get >>= \case
      []   -> empty
      x:xs -> put xs >> return x

data Thread = T { _tState   :: ProgState
                , _tBuffer  :: [Int]
                }
makeClassy ''Thread

type MultiState = V.Vector 2 Thread

-- | Single step through a thread.  Nothing = either the thread terminates,
-- or requires extra input.
stepThread :: StateT Thread Maybe [Int]
stepThread = StateT go
  where
    go :: Thread -> Maybe ([Int], Thread)
    go (T st buf) = (out,) . (`T` buf') <$> t'
      where
        ((t', buf'), out) = runPartB buf
                          . runPromptM interpretB
                          . flip execTapeProg st
                          $ stepTape

-- | Single step through both threads.  Nothing = both threads terminate
stepThreads :: StateT MultiState Maybe [Int]
stepThreads = do
    outA <- zoom (V.ix 0) $ concat <$> many stepThread
    outB <- zoom (V.ix 1) $ concat <$> many stepThread
    V.ix 0 . tBuffer <>= outB
    V.ix 1 . tBuffer <>= outA
    guard . not $ null outA && null outB
    return outB

day18b :: Challenge
day18b (parse->t) = show . length . concat
                  . fromJust
                  $ evalStateT (many stepThreads) ms
  where
    Just ms = V.fromList [ T (PS t (M.singleton 'p' 0)) []
                         , T (PS t (M.singleton 'p' 1)) []
                         ]

