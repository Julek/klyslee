{-# LANGUAGE RankNTypes #-}

import Control.Arrow
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.List.Split

data NNetwork s = Neuron {value :: Int, neurons :: [STRef s (NNetwork s)]} | Ending {value :: Int}

readNN :: String -> ST s (NNetwork s)
readNN file = do
              let splt = map (map (read :: String -> Int) . filter (/=[]) . splitOn " ") . lines $ file
                  nrns = sequence . map (\x -> nrns >>= parseNeuron x) $ splt
              return (Ending 0)

parseNeuron :: [Int] -> [STRef s (NNetwork s)] -> ST s (STRef s (NNetwork s))
parseNeuron (v:ni) ns = do
            if(ni == [])
                  then newSTRef . Ending $ v
                  else newSTRef . Neuron v . map ((!!) ns) $ ni

testNN :: ST s (NNetwork s)
testNN = do
       b <- newSTRef (Ending 0)
       i <- newSTRef (Neuron 0 [b])
       return (Neuron 0 [b, i])

recApp :: (NNetwork s) -> (Int -> Int) -> ST s (NNetwork s)
recApp (Ending n) f = return . Ending . f $ n
recApp (Neuron n nns) f = do
       unns <- sequence . map (\x -> readSTRef x >>= (flip recApp $ f) >>= newSTRef) $ nns
       return (Neuron (f n) unns)

recTot :: (NNetwork s) -> ST s Int
recTot (Ending n) = return n
recTot (Neuron n nns)  = do
       tot <- sequence . map (\x -> readSTRef x >>= recTot) $ nns
       return (n + sum tot)
       
