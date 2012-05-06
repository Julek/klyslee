module Neural where

import Control.Concurrent.MVar

test:: IO ()
test = 
  do
    n <- newMVar 2
    takeMVar n >>= print 
    putMVar n 3
    takeMVar n >>= print 
    return ()
    
{-
data Brain a = Neuron Int [Brain a] (a -> a) | End

evalNetwork :: [a] -> Brain a -> [a]

-}
