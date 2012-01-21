module Klyslee.BreedableInt where

import Klyslee.Breedable
import Klyslee.GeneticsVars
import Klyslee.Utils

import Control.Monad.State
import Data.Bits
import System.Random


instance Breedable Int where
  genRand = doState (randomR (0, 10000))
  mutate x = do
    choice <- (doState  random) :: (RandomGen g, MonadState g m) => m Float
    if(mutation <= choice)
      then
      return x
      else
      do
        offset <- doState (randomR (0, (bitSize x) -1))
        return (complementBit x offset)
  leSexyTime m f = foldM (\ r i -> do
                              choice <- (doState  random) :: (RandomGen g, MonadState g m) => m Float
                              if(choice < 0.5)
                                then
                                return (fst $ xchgBits r f i)
                                else
                                return (fst $ xchgBits r m i)
                          ) 0 [0..((bitSize (0::Int)) - 1)]
