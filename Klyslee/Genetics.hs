module Klyslee.Genetics where

import System.Random
import Klyslee.Utils

mutation = 0.03

class Breadable a where
  leSexyTime :: a -> a -> a
  mutate :: a -> a
  getRandCitizen :: IO a

instance Breadable Int where
  leSexyTime a b = a
  mutate a = a
  getRandCitizen = randomIO :: IO Int

getPopulation :: (Breadable a) => Int -> IO [a]
getPopulation 0 = return []
getPopulation a = (getRandCitizen >>= (\x -> (getPopulation (a-1)) >>= (\y -> return (x:y))))


-- ga :: ()-> () -> 
