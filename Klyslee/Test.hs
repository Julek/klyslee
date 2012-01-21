module Klyslee.Test where

import Klyslee.Breedable
import Klyslee.Notes

import Control.Monad.State
import System.Random

leSexyTimeTest :: (Breedable a, RandomGen g, MonadState g m) => m (a, a, a)
leSexyTimeTest = do
  father <- genRand
  mother <- genRand
  child <- leSexyTime mother father
  return (mother, father, child)
