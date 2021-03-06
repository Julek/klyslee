module Klyslee.Genetics where

import Klyslee.Breedable
import Klyslee.GeneticsVars
import Klyslee.Args
import Klyslee.Utils

import Control.Monad.Random
import Control.Monad.Reader
import Data.List
import Data.List.Split


ga :: (Breedable a, MonadRandom m, MonadReader Bindings m) => (a -> Double) -> Double -> Int -> m a
ga fit goal pops = do 
  init_population <- genPopulation pops
  ga' fit goal pops init_population

ga' :: (Breedable a, MonadRandom m, MonadReader Bindings m) => (a -> Double) -> Double -> Int -> [a] -> m a
ga' fit goal pops pop = do
  let fittest = minimumBy (\x y -> compare (fit x) (fit y)) pop
  if((fit fittest) <= goal)
    then
       return fittest
    else
    do
      tournaments <- replicateM (2*pops) (randSublist pressure pop)
      let survivors = map (minimumBy (\x y -> compare (fit x) (fit y))) tournaments
          parents = map (\ [m,f] -> (m, f)) $ splitEvery 2 survivors
      npop <- mapM (uncurry leSexyTime) parents >>= (mapM mutate)
      ga' fit goal pops npop

genPopulation :: (Breedable a, MonadReader Bindings m, MonadRandom m) => Int -> m [a]
genPopulation n = replicateM n genRand
