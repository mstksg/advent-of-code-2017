{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module AOC2017.Util.Prompt (
  ) where

import           Control.Monad.Prompt
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State

instance MonadPrompt p m => MonadPrompt p (StateT s m) where
    prompt = lift . prompt

instance MonadPrompt p m => MonadPrompt p (MaybeT m) where
    prompt = lift . prompt

