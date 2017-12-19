{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeInType             #-}

module AOC2017.Day18 (day18a, day18b) where

import           AOC2017.Types             (Challenge)
import           Control.Applicative       (many, empty)
import           Control.Lens              (makeClassy, use, at, non, (%=), use, (.=), (<>=), zoom)
import           Control.Monad             (join, guard, when)
import           Control.Monad.Morph       (hoist, lift)
import           Control.Monad.Prompt      (Prompt, prompt, runPromptM)
import           Control.Monad.State       (StateT(..), State, execStateT, evalState, get, put)
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.Monad.Writer      (Writer, runWriter, tell)
import           Data.Char                 (isAlpha)
import           Data.Kind                 (Type)
import           Data.Maybe                (fromJust)
import           Data.Monoid               (First(..))
import qualified Data.Map                  as M
import qualified Data.Vector.Sized         as V

data Tape a = Tape { _tLefts  :: [a]
                   , _tFocus  :: a
                   , _tRights :: [a]
                   }
  deriving Show
makeClassy ''Tape

-- | Shifts the Tape to the left or right by a given amount
move :: Int -> Tape a -> Maybe (Tape a)
move n (Tape ls x rs) = case compare n 0 of
    LT -> case ls of
      []    -> Nothing
      l:ls' -> move (n + 1) (Tape ls' l (x:rs))
    EQ -> Just $ Tape ls x rs
    GT -> case rs of
      []    -> Nothing
      r:rs' -> move (n - 1) (Tape (x:ls) r rs')

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

data ProgState = PS { _psTape :: Tape Op
                    , _psRegs :: M.Map Char Int
                    }

makeClassy ''ProgState

data Command :: Type -> Type where
    CRcv :: Int -> Command Int
    CSnd :: Int -> Command ()

-- Maybe: Move out of bounds
stepTape :: StateT ProgState (MaybeT (Prompt Command)) ()
stepTape = do
    use (psTape . tFocus) >>= \case
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

runTape :: ProgState -> Prompt Command ProgState
runTape ps = fmap fromJust
           . runMaybeT
           . flip execStateT ps
           $ many stepTape

parse :: String -> Tape Op
parse = (\(x:xs) -> Tape [] x xs) . map parseOp . lines

type PartA = StateT (Maybe Int) (Writer (First Int))

runPartA :: PartA a -> Int
runPartA = fromJust . getFirst . snd . runWriter . flip execStateT Nothing

interpretA :: Command a -> PartA a
interpretA = \case
    CRcv x -> do
      when (x /= 0) $
        lift . tell . First =<< get
      return x
    CSnd x -> put (Just x)

day18a :: Challenge
day18a = show
       . runPartA . runPromptM interpretA
       . runTape  . (`PS` M.empty)
       . parse


-- Part B

type PartB = MaybeT (StateT [Int] (Writer [Int]))

runPartB :: [Int] -> PartB a -> ((Maybe a, [Int]), [Int])
runPartB buf = runWriter . flip runStateT buf . runMaybeT

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

stepThread :: Thread -> Maybe ([Int], Thread)
stepThread (T st buf) = (out, T st'' buf') <$ guard (not stopped)
  where
    ((st', buf'), out) = runPartB buf
                       . runPromptM interpretB
                       . runMaybeT
                       . flip runStateT st
                       $ stepTape
    (stopped, st'') = case join st' of
      Nothing         -> (True , st   )
      Just (_, newSt) -> (False, newSt)


runB :: State MultiState [Int]
runB = do
    outA <- zoom (V.ix 0)
          . hoist (return . fromJust)     -- 'many' always succeeds
          $ many (StateT stepThread)
    outB <- zoom (V.ix 1)
          . hoist (return . fromJust)
          $ many (StateT stepThread)
    if null outA && null outB
      then return []
      else do
        V.ix 0 . tBuffer <>= concat outB
        V.ix 1 . tBuffer <>= concat outA
        (concat outB ++) <$> runB


day18b :: Challenge
day18b (parse->t) = show . length $ evalState runB ms
  where
    Just ms = V.fromList [ T (PS t (M.singleton 'p' 0)) []
                         , T (PS t (M.singleton 'p' 1)) []
                         ]
