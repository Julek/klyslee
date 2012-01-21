module Klyslee.Notes where

import Klyslee.Breedable

import System.Random
import Control.Monad.State

data Note = Note Octave AbsNote Intonation

type Octave = Int

data AbsNote = Do | Re | Mi | Fa | Sol | La | Si
                                              deriving(Show)

data Intonation = Norm | Sharp | Flat

instance Show Note where
  show (Note oct note int) = (show note) ++ (show int) ++ (show oct)

{-instance Breedable Note where
  genRand = do
    rand1 <- get
    let (oct, rand2) = (randomR (1, 8) rand1)
        (n, rand3) = randomR (0, 6) rand2
        (i, rand4) = randomR (-1, 1) rand3
        note = getNote n
        int = getIntonation i
    return (Note oct note int)-}
  

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
