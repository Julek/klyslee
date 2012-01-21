module Klyslee.Breedable where

import System.Random
import Control.Monad.State

class Breedable a where
  leSexyTime :: (RandomGen g, MonadState g m) => a -> a -> m a
  mutate :: (RandomGen g, MonadState g m) => a -> m a
  genRand :: (RandomGen g, MonadState g m) => m a
