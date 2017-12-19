{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeInType       #-}

module AOC2017.Day18 (day18a, day18b) where

-- import           Control.Monad.Trans.Control
-- import           Text.Read
import           AOC2017.Types                  (Challenge)
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Morph
import           Control.Monad.Prompt
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Bifunctor
import           Data.Char
import           Data.Foldable
import           Data.Kind
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Debug.Trace
import qualified Data.Map                       as M

addr :: String -> Either Char Int
addr [c] | isAlpha c = Left c
addr str = Right (read str)

type Addr = Either Char Int

data Op = OSnd Addr
        | OBin (Int -> Int -> Int) Char Addr
        | ORcv Char
        | OJgz Addr Addr

instance Show Op where
    show _ = "Op"

parseOp :: String -> Op
parseOp inp = case words inp of
    "snd":(addr->c):_ -> OSnd c
    "set":(x:_):(addr->y):_ -> OBin (const id) x y
    "add":(x:_):(addr->y):_ -> OBin (+) x y
    "mul":(x:_):(addr->y):_ -> OBin (*) x y
    "mod":(x:_):(addr->y):_ -> OBin mod x y
    "rcv":(x:_):_ -> ORcv x
    "jgz":(addr->x):(addr->y):_ -> OJgz x y

data Tape a = Tape { _tLefts  :: [a]
                   , _tFocus  :: a
                   , _tRights :: [a]
                   }
  deriving Show
makeLenses ''Tape

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

data ProgState = PS { _psTape :: Tape Op
                    , _psRegs :: M.Map Char Int
                    }
makeLenses ''ProgState

data Command :: Type -> Type where
    CRcv :: Int -> Command Int
    CSnd :: Int -> Command ()

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
        y <- lift . lift . prompt . CRcv =<< use (psRegs . at x . non 0)
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
day18a (parse->t) = show
                  . runPartA  . runPromptM interpretA
                  . runMaybeT . flip execStateT (PS t M.empty)
                  $ many stepTape

data MultiState = MS { _msProg1   :: ProgState
                     , _msProg2   :: ProgState
                     , _msBuffer1 :: [Int]
                     , _msBuffer2 :: [Int]
                     }

day18b :: Challenge
day18b = undefined

-- type Reg = (Tape Op, M.Map String Int, Maybe Int, [Int])

-- run :: Reg -> Reg
-- run (t@(Tape _ o _),m0,lst,rcv) = case o of
--     OSnd x   -> (move 1 t, m0, Just (lookup x), rcv)
--     -- OSet x y -> (move 1 t, M.insert x (lookup y) m0, lst, rcv)
--     OBin f x y -> (move 1 t, M.insert x (f (M.findWithDefault 0 x m0) (lookup y)) m0, lst, rcv)
--     ORcv c     -> let cVal = M.findWithDefault 0 c m0
--                       rcv' | cVal == 0 = rcv
--                            | otherwise = case lst of
--                                            Nothing -> rcv
--                                            Just s  -> s : rcv
--                   in  (move 1 t, m0, lst, rcv')
--     OJgz x y -> let xVal = lookup x
--                     moveAmt | xVal > 0  = lookup y
--                             | otherwise = 1
--                 in  (move moveAmt t, m0, lst, rcv)
--   where
--     lookup :: Either String Int -> Int
--     lookup (Left r) = M.findWithDefault 0 r m0
--     lookup (Right x) = x

-- day18a = show . head . mapMaybe go . iterate run . (\(x:xs) -> (Tape [] x xs, M.empty, Nothing, [])) . map parseOp . lines
--   where
--     go (_,_,_,rs) = listToMaybe rs

-- data RegB = RegB { _rbTape   :: Tape Op
--                  , _rbRegs   :: M.Map String Int
--                  }

--   deriving Show

-- makeLenses ''RegB

-- -- | Shifts the Tape to the left or right by a given amount
-- move' :: Int -> Tape a -> Maybe (Tape a)
-- move' n (Tape ls x rs) = case compare n 0 of
--     LT -> case ls of
--       []    -> Nothing
--       l:ls' -> move' (n + 1) (Tape ls' l (x:rs))
--     EQ -> Just $ Tape ls x rs
--     GT -> case rs of
--       []    -> Nothing
--       r:rs' -> move' (n - 1) (Tape (x:ls) r rs')

-- runBs :: RegB -> [Int] -> (Maybe RegB, [Int])
-- runBs rb0@(RegB t m0) = case _tFocus t of
--     OSnd x   -> \inp -> maybe noMore (`runBs` inp) (rb0 & rbTape %%~ move' 1)
--     OBin f x y -> \inp -> maybe noMore (`runBs` inp) $
--           rb0 & rbRegs . at x .~ Just (f (M.findWithDefault 0 x m0) (lookup y))
--               & rbTape %%~ move' 1
--     ORcv v -> \case
--         []   -> (Just rb0, [])
--         x:xs -> maybe noMore (`runBs` xs) $ rb0 & rbRegs . at v .~ Just x
--                                                 & rbTape %%~ move' 1
--     OJgz x y -> \inp ->
--       let xVal = lookup x
--           moveAmt | xVal > 0  = lookup y
--                   | otherwise = 1
--       in  maybe noMore (`runBs` inp) $ rb0 & rbTape %%~ move' moveAmt
--   where
--     noMore = (Nothing, [])
--     lookup (Left r) = M.findWithDefault 0 r m0
--     lookup (Right x) = x

-- -- runTwo :: ([Int], [Int]) -> (Maybe RegB, Maybe RegB) -> Maybe (Maybe RegB, Maybe RegB)
-- -- runTwo (b1, b2) = \case
-- --     (Nothing, Nothing ) -> Nothing
-- --     (Just rb1, Nothing) ->

-- -- runBs rb@(RegB t m0) = case _tFocus t of
-- --     OSnd x   -> \inp -> lookup x : maybe [] (`runBs` inp) (rb & rbTape %%~ move' 1)
-- --     OSet x y -> \inp -> maybe [] (`runBs` inp) $ rb & rbRegs . at x .~ Just (lookup y)
-- --                                                     & rbTape %%~ move' 1
-- --     OBin f x y -> \inp -> maybe [] (`runBs` inp) $
-- --         rb & rbRegs . at x .~ Just (f (M.findWithDefault 0 x m0) (lookup y))
-- --            & rbTape %%~ move' 1
-- --     ORcv v -> \case
-- --         []   -> []
-- --         x:xs -> maybe [] (`runBs` xs) $ rb & rbRegs . at v .~ Just x
-- --                                            & rbTape %%~ move' 1
-- --     OJgz x y -> \inp ->
-- --       let xVal = lookup x
-- --           moveAmt | xVal > 0  = lookup y
-- --                   | otherwise = 1
-- --       in  maybe [] (`runBs` inp) $ rb & rbTape %%~ move' moveAmt
-- --   where
-- --     lookup (Left r) = M.findWithDefault 0 r m0
-- --     lookup (Right x) = x

-- -- runBs :: RegB -> [Int] -> [Int]
-- -- runBs rb@(RegB t m0) = case _tFocus t of
-- --     OSnd x   -> \inp -> lookup x : maybe [] (`runBs` inp) (rb & rbTape %%~ move' 1)
-- --     OSet x y -> \inp -> maybe [] (`runBs` inp) $ rb & rbRegs . at x .~ Just (lookup y)
-- --                                                     & rbTape %%~ move' 1
-- --     OBin f x y -> \inp -> maybe [] (`runBs` inp) $
-- --         rb & rbRegs . at x .~ Just (f (M.findWithDefault 0 x m0) (lookup y))
-- --            & rbTape %%~ move' 1
-- --     ORcv v -> \case
-- --         []   -> []
-- --         x:xs -> maybe [] (`runBs` xs) $ rb & rbRegs . at v .~ Just x
-- --                                            & rbTape %%~ move' 1
-- --     OJgz x y -> \inp ->
-- --       let xVal = lookup x
-- --           moveAmt | xVal > 0  = lookup y
-- --                   | otherwise = 1
-- --       in  maybe [] (`runBs` inp) $ rb & rbTape %%~ move' moveAmt
-- --   where
-- --     lookup (Left r) = M.findWithDefault 0 r m0
-- --     lookup (Right x) = x

-- -- runTwoMany :: Tape Op -> [Int]
-- -- runTwoMany t0 = let out1 = runBs rb1 out2
-- --                     out2 = runBs rb2 out1
-- --                 in  out1
-- --   where
-- --     rb1 = RegB t0 (M.singleton "p" 0)
-- --     rb2 = RegB t0 (M.singleton "p" 1)
-- --     -- out1 = runBs rb1 out2
-- --     -- out2 = runBs rb2 out1

-- day18b :: Challenge
-- -- day18b = show . take 3 . runTwoMany . parse
-- day18b = undefined
