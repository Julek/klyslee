module Klyslee.Utils where

import Control.Monad.State
import Data.Bits
import System.Random

randSublist :: (RandomGen g) => Int -> [a] -> State g [a]
randSublist n list = replicateM n (randMember list)
  
randMember :: (RandomGen g) => [a] -> State g a
randMember list = do
  rand <- get
  let (rind, nrand) = randomR (0, length list -1) rand
  put nrand
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
  
{-
import Debug.Trace
import System.Random

randSublist :: Int -> IO [a] -> IO [a]
randSublist 0 list = return []
randSublist n list = rest >>= (\b -> randMember >>= (\f -> return $ f:b))
  where randMember = list >>= (\ls -> (randomIO :: IO Int) >>= (\i -> return $ ls !! (i `mod` (length ls))))
        rest = randSublist (n-1) list

coalesceIO :: [IO a] -> IO [a]
coalesceIO [] = return []
coalesceIO (l:ls) = l >>= (\f -> (coalesceIO ls) >>= (\b -> return $ f:b))

getRandPercentage :: IO Float
getRandPercentage = do
  r <- (randomIO :: IO Int)
  return $ fromIntegral (r `mod` 100) / 100

repeatTimes :: (a -> a) -> a -> Int -> [a]
repeatTimes f arg 0 = []
repeatTimes f arg n = new:(repeatTimes f arg (n-1))
  where new = f arg-}
