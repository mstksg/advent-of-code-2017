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
import           Data.Char                 (isAlpha)
import           Data.Kind                 (Type)
import           Data.Maybe                (fromJust, maybeToList)
import           Data.Monoid               (First(..))
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

-- | Single step through program tape.  Nothing = program terminates (by
-- jumping out of bounds)
stepTape :: StateT ProgState (MaybeT (Prompt Command)) ()
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

-- | Repeatedly run the tape until it goes out of bounds
runTape :: ProgState -> Prompt Command ProgState
runTape ps = fmap fromJust
           . runMaybeT
           . flip execStateT ps
           $ many stepTape

-- | Context in which to interpret Command for Part A
--
-- State parameter is the most resent sent item.  Writer parameter is all
-- of the Rcv'd items.
--
-- State should probably be Accum instead, but Accum is in any usable
-- version of transformers yet.
type PartA = StateT (Maybe Int) (Writer [Int])
runPartA :: PartA a -> Int
runPartA = head . snd . runWriter . flip execStateT Nothing

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
       . runPartA . runPromptM interpretA
       . runTape  . (`PS` M.empty)
       . parse

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
    go (T st buf) = (out, T st'' buf') <$ guard (not stopped)
      where
        ((st', buf'), out) = runPartB buf
                           . runPromptM interpretB
                           . runMaybeT
                           . flip runStateT st
                           $ stepTape
        (stopped, st'') = case join st' of
          Nothing         -> (True , st   )
          Just (_, newSt) -> (False, newSt)

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

