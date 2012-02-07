module Klyslee.Breedable where

import Klyslee.Monad

import Control.Monad.State
import Control.Monad.Random
import Control.Monad.Reader
import System.Random

class Breedable a where
  leSexyTime :: (MonadRandom m, MonadReader Bindings m) => a -> a -> m a
  mutate ::  (MonadRandom m, MonadReader Bindings m) => a -> m a
  genRand :: (MonadRandom m, MonadReader Bindings m) => m a
