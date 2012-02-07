module Klyslee.Monad where

import Control.Monad.Random
import Control.Monad.Reader

data Bindings = Bindings {tuneLength :: Int}

newtype RandReader g a = RandReader { runRandReader :: RandT g (Reader Bindings) a }
  deriving (Monad, MonadReader Bindings, MonadRandom)

evalRandReaderIO :: RandReader StdGen a -> Bindings -> IO a
evalRandReaderIO kly bind = do
  seed <- randomIO
  return . (flip runReader $ bind) . (flip evalRandT $ mkStdGen seed) . runRandReader $ kly
