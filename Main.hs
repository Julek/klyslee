{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Klyslee.BreedableInt
import Klyslee.Genetics
import Klyslee.Notes
import Klyslee.Sound

import Control.Monad.State
import System
import System.Random

main :: IO ()
main = do
  g <- getStdGen
  let mel@(Melody notes) = evalState (ga song_fitness (30.0) 100) g
      freqs = map noteToFreq notes
  print mel
  print $ freqs
  outputWave freqs "temp.wav" 
  system("aplay temp.wav > /dev/null")
  return ()
