module Klyslee.Utils where

import Control.Monad
import Control.Monad.Random
import Data.Bits

randSublist :: (MonadRandom m) => Int -> [a] -> m [a]
randSublist n list = replicateM n (randMember list)
  
randMember :: (MonadRandom m) => [a] -> m a
randMember list = do  
  rind <- getRandomR (0, length list -1)
  return (list!!rind)

xchgBits :: (Bits a) => a -> a -> Int -> (a, a)
xchgBits a1 a2 n = (setBitTo a1 n (testBit a2 n), setBitTo a2 n (testBit a1 n))

setBitTo :: (Bits a) => a -> Int -> Bool -> a
setBitTo a n True = setBit a n
setBitTo a n False = clearBit a n
