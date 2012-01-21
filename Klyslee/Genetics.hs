module Klyslee.Genetics where

import Klyslee.Breedable
import Klyslee.GeneticsVars
import Klyslee.Utils

import Control.Monad.State
import Data.List
import Data.List.Split
import System.Random
      

ga :: (Breedable a, RandomGen g) => (a -> Double) -> Double -> Int -> State g a
ga fit goal pops = do 
  rand1 <- get
  let (init_population, rand2) = runState (genPopulation pops) rand1
  put rand2
  ga' fit goal pops init_population

ga' :: (Breedable a, RandomGen g) => (a -> Double) -> Double -> Int -> [a] -> State g a
ga' fit goal pops pop = do
  let fittest = minimumBy (\x y -> compare (fit x) (fit y)) pop
  if((fit fittest) <= goal)
    then
       return fittest
    else
    do
      rand1 <- get 
      let (tournaments, rand2) = runState (replicateM (2*pops) (randSublist pressure pop)) rand1
          survivors = map (minimumBy (\x y -> compare (fit x) (fit y))) tournaments
          parents = map (\ [m,f] -> (m, f)) $ splitEvery 2 survivors
      npop <- mapM (uncurry leSexyTime) parents >>= (mapM mutate)
      ga' fit goal pops npop

genPopulation :: (Breedable a, RandomGen g) => Int -> State g [a]
genPopulation n = replicateM n genRand
