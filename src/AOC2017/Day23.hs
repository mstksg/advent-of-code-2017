{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module AOC2017.Day23 (day23a, day23b) where

import           AOC2017.Types             (Challenge)
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
import           System.IO.Unsafe
import           Text.Pretty.Simple
import           Text.Read                 (readMaybe)
import qualified Data.IntMap               as IM
import qualified Data.IntSet               as IS
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import qualified Data.Text.Lazy            as TL
import qualified Data.Vector.Sized         as V
import qualified Linear                    as L

type Addr = Either Char Int

addr :: String -> Addr
addr [c] | isAlpha c = Left c
addr str = Right (read str)

data BinOp = BOSet | BOSub | BOMul
  deriving Show
makePrisms ''BinOp

runBO :: BinOp -> Int -> Int -> Int
runBO BOSet = const id
runBO BOSub = (-)
runBO BOMul = (*)

data Op = OBin BinOp Char Addr
        | OJnz Addr Int
  deriving Show
makePrisms ''Op

parseOp :: String -> Op
parseOp inp = case words inp of
    "set":(x:_):(addr->y):_     -> OBin BOSet x y
    "sub":(x:_):(addr->y):_     -> OBin BOSub x y
    "mul":(x:_):(addr->y):_     -> OBin BOMul x y
    "jnz":(addr->x):(read->y):_ -> OJnz x y
    _                           -> error "Bad parse"

parse :: String -> Tape (Int,Op)
parse = unsafeTape . zip [0..] . map parseOp . lines

data ProgState = PS { _psTape :: Tape (Int, Op)
                    , _psRegs :: M.Map Char Int
                    }

makeClassy ''ProgState

data Command :: Type -> Type where
    CCnt :: Int -> Command ()
    CAsk :: String -> Command (Maybe (Int, M.Map Char Int))

-- | Context in which Tape commands are run.  Tape commands have access to
-- an underlying 'Prompt Command' effect monad that allows it to 'Rcv' and
-- 'Snd'.
--
-- Nothing = program terminates by running out of bounds
type TapeProg = MaybeT (StateT ProgState (Prompt Command))
execTapeProg :: TapeProg a -> ProgState -> Prompt Command ProgState
execTapeProg tp ps = flip execStateT ps . runMaybeT $ tp

runTapeProg :: TapeProg a -> ProgState -> Prompt Command (Maybe a, ProgState)
runTapeProg tp ps = flip runStateT ps . runMaybeT $ tp

-- | Single step through program tape.
stepTape :: TapeProg ()
stepTape = use (psTape . tFocus . _2) >>= \case
    OBin bo x y -> do
      yVal <- addrVal y
      psRegs . at x . non 0 %= \x -> runBO bo x yVal
      forMOf_ _BOMul bo $ \() ->
        lift . lift . prompt . CCnt $ 1
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

-- | Context in which to interpret Command for Part A
--
-- State parameter is the most recent sent item.  Writer parameter is all
-- of the Rcv'd items.
--
-- State should probably be Accum instead, but Accum is in any usable
-- version of transformers yet.
type PartA = StateT (Maybe Int) (Writer (Sum Int))
execPartA :: PartA a -> Int
execPartA = getSum . snd . runWriter . flip execStateT Nothing

-- | Interpet Command for Part A
interpretA :: Command a -> PartA a
interpretA = \case
    CCnt n -> lift . tell $ Sum n
    CAsk n -> return (Just (1, M.empty))

type PartB = StateT (Maybe Int) (WriterT (Sum Int) IO)
runPartB :: PartB a -> IO a
runPartB = fmap (fst . fst) . runWriterT . flip runStateT Nothing

-- | Interpet Command for Part B
interpretB :: Command a -> PartB a
interpretB = \case
    CCnt n -> lift . tell $ Sum n
    CAsk n -> liftIO $ do
        putStrLn n
        parsePrompt . words <$> getLine
  where
    parsePrompt xs = do
        y:ys <- return xs
        y' <- readMaybe y
        changes <- forM ys $ \case
          [] -> Nothing
          c:cs -> do
            cs' <- readMaybe cs
            return (c, cs')
        return (y', M.fromList changes)

day23a :: Challenge
day23a = show
       . execPartA . runPromptM interpretA
       . execTapeProg (many stepTape)    -- stepTape until program terminates
       . (`PS` M.empty) . parse

day23b :: Challenge
day23b = TL.unpack . pShow . toDay23 . IM.fromList . toList . parse
-- day23b :: Challenge
-- day23b = show . view (psRegs . at 'p' . non 0) . unsafePerformIO . runPartB
--        . runPromptM interpretB
--        . execTapeProg stepInter
--        -- . (`PS` m0) . fromJust . move 12 . parse
--        . (`PS` m0) . parse
--   where
--     m0 = M.singleton 'a' 1
--     stepInter :: TapeProg ()
--     stepInter = do
--       regs <- use psRegs
--       currIx <- use $ psTape . tFocus . _1
--       let out = (currIx, map (\x -> (x, regs ^. at x . non 0)) ['a' .. 'h'])
--       (n,mp) <- fmap (fromMaybe (1, M.empty)) . lift . lift . prompt $ CAsk (show out)
--       psRegs %= M.union mp
--       res <- replicateM n (optional stepTape)
--       case sequence res of
--           Nothing -> return ()
--           Just _  -> stepInter

    -- m0 = M.fromList $ zip ['a'..] [1,109900,126900,10,71833,0,2,0]
    -- m0 = M.fromList $ zip ['a'..] [1,109900,126900,7,93038,0,7,0]
    -- log = do
    --   regs <- use psRegs
    --   currIx <- use $ psTape . tFocus . _1
    --   out  <- traceShowId <$> return (currIx, map (\x -> (x, regs ^. at x . non 0)) ['a' .. 'h'])
    --   guard (length (snd out) > 1)
      -- traceShowId <$> use (psRergs .)

data BOTree = BOLeaf Addr
            | BOAp BinOp BOTree BOTree
  deriving Show

data Day23 = DWhile Addr Day23
           | DIf Char Day23
           | DSet (M.Map Char BOTree)
           | Day23 :+ Day23
           | DTerm
           | DDebug (IM.IntMap Op)
           -- | DDebug String
  deriving Show

toDay23 :: IM.IntMap Op -> Day23
toDay23 opMap = case minWhile of
    Nothing       -> DDebug opMap
    Just (i,src) -> case IM.splitLookup i opMap of
      (befJump, Just jumpTarg, aftJump)
        | IM.null befJump -> -- loop starts here
            case lookupIMMax src of
              Nothing -> error "huhh"
              Just (bigLoopI, bigLoopC) -> do
                case IM.splitLookup bigLoopI (IM.insert i jumpTarg aftJump) of
                  (inWhile, Just _, aftWhile) ->
                    DWhile bigLoopC (toDay23 inWhile)
                      :+ toDay23 aftWhile
            -- mkWhiles src (IM.insert i jumpTarg aftJump)
        | otherwise -> -- stuff before loop
            -- let aftJump' = aftJump `IM.difference` src
            -- in  DDebug befJump :+ toDay23 aftJump'
            DDebug befJump :+ toDay23 (IM.insert i jumpTarg aftJump)
        -- case jumpTarg of
        --   Nothing ->
        -- let next = case jumpTarg of
        --              Nothing -> aftJump
        --              Just jt -> IM.insert i jt aftJump
        -- in  if | IM.null befJump -> DDebug next
        --        | IM.null next    -> DDebug befJump
        --        | otherwise       -> DSeq (DDebug befJump) (toDay23 next)
  where
    (jumps, noJumps) = flip IM.mapEither opMap $ \case
        OBin bo r x -> Right (bo, r, x)
        OJnz r j    -> Left (r, j)
    (ifs, whiles) = IM.partition ((> 0) . snd) jumps
    ifMap = IM.fromList
          . map (\(i, (r, j)) -> (i, (r, i + j)))
          $ IM.toList ifs
    whileMap = IM.fromListWith IM.union
             . map (\(i,(r, j)) -> (i + j, IM.singleton i r))
             $ IM.toList whiles
    minWhile = lookupIMMin whileMap
    -- mkWhiles :: IM.IntMap Addr -> IM.IntMap Op -> Day23
    -- mkWhiles = 

lookupIMMin :: IM.IntMap a -> Maybe (Int, a)
lookupIMMin m | IM.null m = Nothing
              | otherwise = Just $ IM.findMin m

lookupIMMax :: IM.IntMap a -> Maybe (Int, a)
lookupIMMax m | IM.null m = Nothing
              | otherwise = Just $ IM.findMax m
