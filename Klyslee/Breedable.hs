module Klyslee.Breedable where

import Klyslee.Args

import Control.Monad.Random
import Control.Monad.Reader

class Breedable a where
  leSexyTime :: (MonadRandom m, MonadReader Bindings m) => a -> a -> m a
  mutate ::  (MonadRandom m, MonadReader Bindings m) => a -> m a
  genRand :: (MonadRandom m, MonadReader Bindings m) => m a
