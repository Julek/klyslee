module Klyslee.Utils where

import Control.Monad.State
import Data.Bits
import System.Random

randSublist :: (RandomGen g) => Int -> [a] -> State g [a]
randSublist n list = replicateM n (randMember list)
  
randMember :: (RandomGen g) => [a] -> State g a
randMember list = do  
  rind <- doState $ randomR (0, length list -1)
  return (list!!rind)
  
doState :: MonadState s m => ( s -> ( a, s ) ) -> m a
doState f = do
  s <- get
  let ( r, s' ) = f s
  put s'
  return r
  

xchgBits :: (Bits a) => a -> a -> Int -> (a, a)
xchgBits a1 a2 n = (setBitTo a1 n (testBit a2 n), setBitTo a2 n (testBit a1 n))

setBitTo :: (Bits a) => a -> Int -> Bool -> a
setBitTo a n True = setBit a n
setBitTo a n False = clearBit a n
