import Klyslee.BreedableInt
import Klyslee.Genetics
-- import Klyslee.Notes

import Control.Monad.State
import System.Random

main :: IO ()
main = do
  g <- getStdGen
  print $ (evalState (ga (\x -> abs $ (fromIntegral x) - 3) (2.0) 10) g :: Int)
