{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module AOC2017.Util.Prompt (
  ) where

import           Control.Monad.Prompt
import           Control.Monad.State
import           Control.Monad.Trans.Class
import           Control.Lens
import           Control.Monad.Trans.Maybe

instance MonadPrompt p m => MonadPrompt p (StateT s m) where
    prompt = lift . prompt

instance MonadPrompt p m => MonadPrompt p (MaybeT m) where
    prompt = lift . prompt

instance MonadState s m => MonadState s (PromptT p m) where
    get = lift get
    put = lift . put
    state = lift . state

-- instance Zoom m n s t => Zoom (PromptT p m) (PromptT p n) s t where
