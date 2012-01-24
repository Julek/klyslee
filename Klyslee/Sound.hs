module Klyslee.Sound where
import Data.WAVE
import Data.Int (Int32)
import Data.List.Split (splitOn)

samplesPS = 48000
bitrate = 32

header = WAVEHeader 1 samplesPS bitrate Nothing

sound :: Double -> Int -> Double -> Int32 -> [Int32]
sound freq samples len volume = take (round $ len * (fromIntegral samples)) $ 
                         map (round . (* fromIntegral volume)) $
                         map sin [0.0, (freq * 2 * pi / (fromIntegral samples))..]

waveData = WAVE header

makeWavFile :: WAVE -> String -> IO ()
makeWavFile wav name = putWAVEFile name wav

outputWave :: [Double] -> String -> IO()
outputWave freqs name = makeWavFile (waveData $ foldl1 (++) $ map (\x -> map (:[]) $ sound x samplesPS 0.5 (maxBound `div` 2)) freqs) name
