module Klyslee.Genetics where

import Data.Bits
import Data.List
import Data.List.Split
import Debug.Trace
import Klyslee.Utils
import System.Random

mutation = 0.03 :: Float

class Breadable a where
  leSexyTime :: a -> a -> IO a
  mutate :: a -> IO a
  getRandCitizen :: IO a

instance Breadable Int where
  leSexyTime a b = foldl (\x n -> x >>= (\t -> (getRandPercentage) >>= (\p -> return $ if(p < 0.5) 
                                                                                         then
                                                                                         fst $ xchgBits t b n
                                                                                       else
                                                                                         t))) (return a) [0..((bitSize a) - 1)]
  mutate a = (randomIO :: IO Int) >>= (\x -> return $ complementBit a (x `mod` (bitSize a)))
  getRandCitizen = (randomIO :: IO Int) >>= (\x -> return $ x `mod` 100)

getPopulation :: (Breadable a) => Int -> IO [a]
getPopulation 0 = return []
getPopulation a = (getRandCitizen >>= (\x -> (getPopulation (a-1)) >>= (\y -> return (x:y))))

mutatePopulation :: (Breadable a) => IO [a] -> IO [a]
mutatePopulation pop = pop >>= (\ls -> coalesceIO $ map (\x -> mutateMember x) ls)

mutateMember :: (Breadable a) => a -> IO a
mutateMember member = do
  test <- getRandPercentage
  if(test < mutation)
    then
    mutate member
    else
    return member
  

ga :: (Breadable a, Show a) => (a -> Double) -> (a -> Bool) -> IO a
ga fitness goal = ga' fitness goal (getPopulation 1000)

ga' :: (Breadable a, Show a) => (a -> Double) -> (a -> Bool) -> IO [a] -> IO a
ga' fitness goal population = do
  pop <- population
  parents <- (coalesceIO $ repeatTimes (randSublist 5) population (length pop `div` 2)) >>= (\l -> return $ map (\x -> head $ sortBy (\ a b -> compare (fitness b) (fitness a)) x) l)
  npop <- mutatePopulation . coalesceIO $ foldl1 (++) $ map (\x -> [leSexyTime (head x) (head . tail $ x), leSexyTime (head x) (head . tail $ x) ]) $ takeWhile (\x -> length x == 2) $! splitEvery 2 $! parents
  let best = head $ sortBy (\ a b -> compare (fitness b) (fitness a)) npop
  if(goal best) 
    then
     return best
    else
     ga' fitness goal (return npop)
