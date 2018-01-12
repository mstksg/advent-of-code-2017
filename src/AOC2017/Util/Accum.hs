{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module AOC2017.Util.Accum (
    MonadAccum(..)
  , A.AccumT(..)
  , A.execAccumT
  , A.evalAccumT
  , A.Accum
  , A.accum
  , A.runAccum
  , A.execAccum
  , A.evalAccum
  ) where

import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State
import           Control.Monad.Writer
import qualified Control.Monad.Trans.Accum as A

instance (Monoid a, Monoid w, MonadWriter w m) => MonadWriter w (A.AccumT a m) where
    tell   = lift . tell
    listen m = A.AccumT $ fmap (\case ~((a,s),w) -> ((a,w),s)) . listen . A.runAccumT m
    pass   m = A.AccumT $ pass . fmap (\case ~((a,f),s) -> ((a,s),f)) . A.runAccumT m

class (Monoid w, Monad m) => MonadAccum w m | m -> w where
    add  :: w -> m ()
    look :: Monoid w => m w

instance (Monoid w, Monad m) => MonadAccum w (A.AccumT w m) where
    add  = A.add
    look = A.look

instance MonadAccum w m => MonadAccum w (MaybeT m) where
    add  = lift . add
    look = lift look

instance MonadAccum w m => MonadAccum w (StateT s m) where
    add  = lift . add
    look = lift look

