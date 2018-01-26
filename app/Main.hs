module Main where

import Euterpea
import Player
import MusicBuilder

main :: IO ()
main = do
                putStrLn "Welcome to Haskelody!"
                playScale C Maj qn 4
                playScale D Min en 5
                playScaleWithDrums E Min hn 3
                playScaleWithChord F Maj qn 4
                playScaleWithChord G Maj qn 4
                playScaleWithChord A Min qn 4
                return ()
        
