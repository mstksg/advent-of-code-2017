{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeOperators         #-}

module AOC2017.Day18 (day18a, day18b) where

import           AOC2017.Types             (Challenge)
import           AOC2017.Util.Accum        (AccumT(..), execAccumT, look, add)
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Fail
import           Control.Monad.Prompt      (Prompt, prompt, runPromptM)
import           Control.Monad.State       (MonadState, StateT(..), State, execStateT, evalState)
import           Control.Monad.Trans.Class (MonadTrans(lift))
import           Control.Monad.Trans.Maybe (MaybeT(..))
import           Control.Monad.Writer
import           Data.Char                 (isAlpha)
import           Data.Kind                 (Type)
import           Data.Maybe                (fromJust)
import           Data.Monoid               (First(..), Last(..))
import           Data.Type.Disjunction
import qualified Data.List.PointedList     as P
import qualified Data.Map                  as M
import qualified Data.Vector.Sized         as V

{-
******************
*  The Language  *
******************
-}

type Addr = Either Char Int

addr :: String -> Addr
addr [c] | isAlpha c = Left c
addr str = Right (read str)

data Op = OSnd Addr
        | OBin (Int -> Int -> Int) Char Addr
        | ORcv Char
        | OJgz Addr Addr

parseOp :: String -> Op
parseOp inp = case words inp of
    "snd":(addr->c):_           -> OSnd c
    "set":(x:_):(addr->y):_     -> OBin (const id) x y
    "add":(x:_):(addr->y):_     -> OBin (+)        x y
    "mul":(x:_):(addr->y):_     -> OBin (*)        x y
    "mod":(x:_):(addr->y):_     -> OBin mod        x y
    "rcv":(x:_):_               -> ORcv x
    "jgz":(addr->x):(addr->y):_ -> OJgz x y
    _                           -> error "Bad parse"

parse :: String -> P.PointedList Op
parse = fromJust . P.fromList . map parseOp . lines

{-
**************************
*  The Abstract Machine  *
**************************
-}

data Memory :: Type -> Type where
    MGet :: Char -> Memory Int
    MSet :: Char -> Int -> Memory ()
    MMov :: Int -> Memory ()
    MPk  :: Memory Op

-- | Abstract data type describing "IO" available to the abstract machine
data Command :: Type -> Type where
    CRcv :: Int -> Command Int    -- ^ input is current value of buffer
    CSnd :: Int -> Command ()     -- ^ input is thing being sent

type Machine = Prompt (Memory :|: Command)

cRcv :: Int -> Machine Int
cRcv = prompt . R . CRcv
cSnd :: Int -> Machine ()
cSnd = prompt . R . CSnd
cGet :: Char -> Machine Int
cGet = prompt . L . MGet
cSet :: Char -> Int -> Machine ()
cSet r = prompt . L . MSet r
cMov :: Int -> Machine ()
cMov = prompt . L . MMov
cPk  :: Machine Op
cPk  = prompt $ L MPk

data ProgState = PS { _psTape :: P.PointedList Op
                    , _psRegs :: M.Map Char Int
                    }
makeClassy ''ProgState

-- | Single step through program tape.
stepTape :: Machine ()
stepTape = cPk >>= \case
    OSnd x -> do
      cSnd =<< addrVal x
      cMov 1
    OBin f x y -> do
      yVal <- addrVal y
      cSet x . (`f` yVal) =<< cGet x
      cMov 1
    ORcv x -> do
      y <- cRcv =<< cGet x
      cSet x y
      cMov 1
    OJgz x y -> do
      xVal <- addrVal x
      cMov =<< if xVal > 0
                 then addrVal y
                 else return 1
  where
    addrVal (Left r ) = cGet r
    addrVal (Right x) = return x

interpMem
    :: (MonadState s m, MonadPlus m, HasProgState s)
    => Memory a
    -> m a
interpMem = \case
    MGet c   -> use (psRegs . at c . non 0)
    MSet c x -> psRegs . at c . non 0 .= x
    MMov n   -> do
      Just t' <- P.moveN n <$> use psTape
      psTape .= t'
    MPk      -> use (psTape . P.focus)

{-
************************
*  Context for Part A  *
************************
-}

type PartA = MaybeT (StateT ProgState (AccumT (Last Int) (Writer (First Int))))

execPartA :: PartA a -> ProgState -> Int
execPartA p s = fromJust . getFirst . execWriter
              . flip execAccumT mempty
              . flip execStateT s
              . runMaybeT
              $ p

-- | Interpet Command for Part A
interpA :: Command a -> PartA a
interpA = \case
    CRcv x -> lift . lift $ do
      when (x /= 0) $
        tell . First . getLast =<< look
      return x
    CSnd x -> lift . lift $
      add (Last (Just x))

day18a :: Challenge
day18a = show
       . execPartA (many . runPromptM (interpMem >|< interpA) $ stepTape)
       . (`PS` M.empty)
       . parse

{-
************************
*  Context for Part B  *
************************
-}

data Thread = T { _tState   :: ProgState
                , _tBuffer  :: [Int]
                }
makeClassy ''Thread

instance HasProgState Thread where
    progState = tState

-- | Interpet Command for Part B, with an [Int] writer side-channel
interpB
    :: (MonadFail m, MonadState s m, HasThread s)
    => Command a
    -> WriterT [Int] m a
interpB = \case
    CSnd x -> tell [x]
    CRcv _ -> do
      x:xs <- use tBuffer
      tBuffer .= xs
      return x

type MultiState = V.Vector 2 Thread

-- | Single step through both threads.  Nothing = both threads terminate
stepThreads
    :: MaybeT (State MultiState) Int
stepThreads = do
    outA <- execWriterT $
      zoom (V.ix 0) . many $ runPromptM (interpMem >|< interpB) stepTape
    outB <- execWriterT $
      zoom (V.ix 1) . many $ runPromptM (interpMem >|< interpB) stepTape
    V.ix 0 . tBuffer <>= outB
    V.ix 1 . tBuffer <>= outA
    guard . not $ null outA && null outB
    return $ length outB

day18b :: Challenge
day18b (parse->t) = show . sum . concat
                  . evalState (runMaybeT (many stepThreads))
                  $ ms
  where
    Just ms = V.fromList [ T (PS t (M.singleton 'p' 0)) []
                         , T (PS t (M.singleton 'p' 1)) []
                         ]

