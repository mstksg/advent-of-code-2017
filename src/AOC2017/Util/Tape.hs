{-# LANGUAGE DeriveFunctor          #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TemplateHaskell        #-}

module AOC2017.Util.Tape (
    Tape(..)
  , HasTape(..)
  , toTape, listToTape, unsafeTape
  , moveLeft, moveRight
  , move
  , moveLeftC, moveRightC
  , moveC
  , moveRightD, moveLeftD
  ) where

import           AOC2017.Util
import           Control.Comonad
import           Control.Lens
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Maybe
import           Data.Monoid
import qualified Data.List.NonEmpty as NE

data Tape a = Tape { _tLefts  :: ![a]
                   , _tFocus  :: !a
                   , _tRights :: ![a]
                   }
  deriving (Show, Functor)
makeClassy ''Tape

instance Foldable Tape where
    foldMap f (Tape ls x rs) = foldMap f (reverse ls) <> f x <> foldMap f rs
instance Traversable Tape where
    traverse f (Tape ls x rs) = Tape <$> (reverse <$> traverse f (reverse ls))
                                     <*> f x
                                     <*> traverse f rs

instance Comonad Tape where
    extract   = _tFocus
    duplicate t = Tape ls t rs
      where
        _:ls = iterateMaybe moveLeft t
        _:rs = iterateMaybe moveRight t

toTape :: NonEmpty a -> Tape a
toTape (x :| xs) = Tape [] x xs

listToTape :: [a] -> Maybe (Tape a)
listToTape []     = Nothing
listToTape (x:xs) = Just (Tape [] x xs)

unsafeTape :: [a] -> Tape a
unsafeTape = fromMaybe (error "unsafeTape: Empty list") . listToTape

moveLeft :: Tape a -> Maybe (Tape a)
moveLeft (Tape ls x rs) = case ls of
    []    -> Nothing
    l:ls' -> Just (Tape ls' l (x:rs))

moveRight :: Tape a -> Maybe (Tape a)
moveRight (Tape ls x rs) = case rs of
    []    -> Nothing
    r:rs' -> Just (Tape (x:ls) r rs')

-- | Shifts the Tape to the left or right by a given amount
move :: Int -> Tape a -> Maybe (Tape a)
move n = case compare n 0 of
    LT -> (!!! abs n) . iterate (moveLeft  =<<) . Just
    EQ -> Just
    GT -> (!!! n    ) . iterate (moveRight =<<) . Just

-- | One step to the right, but cycling
moveLeftC :: Tape a -> Tape a
moveLeftC (Tape ls x rs) = case ls of
    [] -> let r :| rs' = NE.reverse (x :| rs)
          in  Tape rs' r []
    l:ls' -> Tape ls' l (x:rs)

-- | One step to the right, but cycling
moveRightC :: Tape a -> Tape a
moveRightC (Tape ls x rs) = case rs of
    [] -> let l :| ls' = NE.reverse (x :| ls)
          in  Tape [] l ls'
    r:rs' -> Tape (x:ls) r rs'

-- | Shifts the Tape to the left or right by a given amount, cyclicly
moveC :: Int -> Tape a -> Tape a
moveC n = case compare n 0 of
    LT -> (!!! abs n) . iterate moveLeftC
    EQ -> id
    GT -> (!!! n    ) . iterate moveRightC

moveLeftD :: a -> Tape a -> Tape a
moveLeftD d (Tape ls x rs) = case ls of
    []    -> Tape [] d (x:rs)
    l:ls' -> Tape ls' l (x:rs)

moveRightD :: a -> Tape a -> Tape a
moveRightD d (Tape ls x rs) = case rs of
    []    -> Tape (x:ls) d []
    r:rs' -> Tape (x:ls) r rs'
