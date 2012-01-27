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
                              let from = if(choice < 0.5)
                                         then
                                           f
                                         else
                                           m
                              return (fst $ xchgBits r from i)
                          ) 0 [0..((bitSize (0::Int)) - 1)]
