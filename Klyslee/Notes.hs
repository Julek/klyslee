module Klyslee.Notes where

import Klyslee.Breedable
import Klyslee.GeneticsVars
import Klyslee.Utils

import System.Random
import Control.Monad.State

tunel = 4 :: Int

newtype Melody = Melody [Note]

data Note = Note Octave AbsNote Intonation

type Octave = Int

data AbsNote = Do | Re | Mi | Fa | Sol | La | Si
                                              deriving(Show)

data Intonation = Norm | Sharp | Flat


instance Breedable Melody where
  genRand = replicateM tunel (do
    oct <- doState (randomR (1, 8))
    n <-doState (randomR (0, 6))
    i <- doState (randomR (-1, 1))
    let note = getNote n
        int = getIntonation i
    return (Note oct note int)) >>= (return . Melody)

  leSexyTime (Melody m) (Melody f) = foldM (\r i -> do
                              choice <- (doState  random) :: (RandomGen g, MonadState g m) => m Float
                              let from = if(choice < 0.5)
                                         then
                                           f
                                         else
                                           m
                              return (r ++ [from!!i])) [] [0..(tunel - 1)] >>= (return . Melody)

  mutate x = do
    choice <- (doState  random) :: (RandomGen g, MonadState g m) => m Float
    if(mutation <= choice)
      then
        return x
      else
      do
        return x

instance Show Melody where
  show (Melody ls) = " " ++ (foldl1 (\x y -> x ++ " " ++ y) $ map (show) ls)

instance Show Note where
  show (Note oct note int) = (show note) ++ (show int) ++ (show oct)

instance Show Intonation where
  show Norm = ""
  show Sharp = "#"
  show Flat = "♭"

getIntonation :: Int -> Intonation
getIntonation  (-1) = Flat
getIntonation 0 = Norm
getIntonation 1 = Sharp

getNote :: Int -> AbsNote
getNote 0 = Do
getNote 1 = Re
getNote 2 = Mi
getNote 3 = Fa
getNote 4 = Sol
getNote 5 = La
getNote 6 = Si
