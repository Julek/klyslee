module Klyslee.Notes where

import Klyslee.Args
import Klyslee.Breedable
import Klyslee.GeneticsVars
import Klyslee.Utils

import Data.List
import Data.Maybe
import Control.Monad.Random
import Control.Monad.Reader
import Control.Monad.State
import System.Random

range = (4, 6)

intervals = [((Do, Norm), [(Mi, Norm), (Mi, Flat), (Sol, Norm)]), ((Re, Flat), [(Fa, Norm), (Fa, Flat), (La, Flat)]), ((Re, Norm), [(Sol, Flat), (Fa, Norm), (La, Norm)]), ((Mi, Flat), [(Sol, Norm), (Sol, Flat), (Si, Flat)]), ((Mi, Norm), [(La, Flat), (Sol, Norm), (Si, Norm)]), ((Fa, Norm), [(La, Norm), (La, Flat), (Do, Norm)]), ((Sol, Flat), [(Si, Flat), (La, Norm), (Re, Flat)]), ((Sol, Norm), [(Si, Norm), (Si, Flat), (Re, Norm)]), ((La, Flat), [(Do, Norm), (Si, Norm), (Mi, Flat)]), ((La, Norm), [(Re, Flat), (Do, Norm), (Mi, Norm)]), ((Si, Flat), [(Re, Norm), (Re, Flat), (Fa, Norm)]), ((Si, Norm), [(Mi, Flat), (Re, Norm), (Sol, Flat)])]

newtype Melody = Melody [Note]

data Note = Note Octave AbsNote Intonation
            deriving(Eq)
                    
data Harmonic = Harmonic Octave AbsNote Intonation
            deriving(Eq)
                    
type Octave = Int

type Duration = Double

data AbsNote = Do | Re | Mi | Fa | Sol | La | Si
                                              deriving(Show, Eq)

data Intonation = Norm | Flat
                         deriving(Eq)


instance Breedable Melody where
  genRand = do
    tunel <- ask >>= return . tuneLength 
    replicateM tunel (do
                         oct <- getRandomR range
                         n <-getRandomR (0, 6)
                         i <- getRandomR (-1, 0)
                         let note = getNote n
                             int = getIntonation i
                         return (standardise $ Note oct note int)) >>= (return . Melody)

  leSexyTime (Melody m) (Melody f) = do
    tunel <- ask >>= return . tuneLength
    foldM (\r i -> do
              choice <- getRandom :: (MonadRandom m) => m Float
              let from = if(choice < 0.5)
                         then
                           f
                         else
                           m
              return (r ++ [from!!i])) [] [0..(tunel - 1)] >>= (return . Melody)

  mutate s@(Melody x) = do
    choice <- getRandom :: (MonadRandom m) => m Float
    if(mutation <= choice)
      then
        return s
      else
      do
        tunel <- ask >>= return . tuneLength 
        ind <- getRandomR (0, tunel - 1)
        oct <- getRandomR range
        n <- getRandomR (0, 6)
        i <- getRandomR (-1, 0)
        let note = getNote n
            int = getIntonation i
            (f, _:b) = splitAt ind x
        return (Melody $ f ++ (standardise $ Note oct note int):b)
                      
                                     

instance Show Melody where
  show (Melody ls) = " " ++ (foldl1 (\x y -> x ++ " " ++ y) $ map (show) ls)

instance Show Note where
  show (Note oct note int) = (show note) ++ (show int) ++ (show oct)

instance Show Intonation where
  show Norm = ""
  show Flat = "♭"

getIntonation :: Int -> Intonation
getIntonation  (-1) = Flat
getIntonation 0 = Norm

getNote :: Int -> AbsNote
getNote 0 = Do
getNote 1 = Re
getNote 2 = Mi
getNote 3 = Fa
getNote 4 = Sol
getNote 5 = La
getNote 6 = Si

fromNote :: AbsNote -> Int
fromNote Do = 0
fromNote Re = 1
fromNote Mi = 2
fromNote Fa = 3
fromNote Sol = 4
fromNote La = 5
fromNote Si = 6

standardise :: Note -> Note
standardise (Note oct Fa Flat) = Note oct Mi Norm
standardise (Note oct Do Flat) = Note oct Si Norm
standardise x = x

song_fitness :: Melody -> Double
song_fitness (Melody ls) = sum $ unfoldr interval_fitness_wrapper ls

interval_fitness :: Note -> Note -> Double
interval_fitness sh@(Note oct1 note1 int1) (Note oct2 note2 int2) = interval_penalty + octave_penalty
  where interval_penalty = if(elem (note2, int2) $ harmonic)
                           then
                             0
                           else
                             1
        octave_penalty = if(abs(oct1 - oct2) < 2)
                         then
                           0
                         else
                           1
        harmonicl = lookup (note1, int1) intervals
        harmonic = if(isJust harmonicl)
                   then
                     fromJust harmonicl
                   else
                     error ("Invalid note: " ++ show sh)
        
interval_fitness_wrapper :: [Note] -> Maybe (Double, [Note])
interval_fitness_wrapper (n1:n2:ns) = Just (interval_fitness n1 n2, n2:ns)
interval_fitness_wrapper x = Nothing

noteToFreq :: Note -> Double
noteToFreq st@(Note oct note int) =
  440 * (2 ** (((fromIntegral (12*(oct - 1) + 4) :: Double) + (fromIntegral . fromNote $ note :: Double) - 49)/12))
