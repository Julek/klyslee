{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Klyslee.Genetics
import Klyslee.Args
import Klyslee.Monad
import Klyslee.Notes
import Klyslee.Sound

import Control.Monad.State
import System
import System.Random

main :: IO ()
main = do
  args <- getArgs
  let len = if(args == [])
               then 4
               else ((read . head $ args) :: Int)
  mel@(Melody notes) <- evalRandReaderIO (ga song_fitness (30.0) 100) (Bindings {tuneLength = len})
  let freqs = map noteToFreq notes
  print mel
  print $ freqs
  outputWave freqs "temp.wav" 
  system("aplay temp.wav > /dev/null")
  return ()
