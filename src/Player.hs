module Player where

import Euterpea
import MusicBuilder

playScale :: PitchClass -> ScaleType -> Dur -> Octave -> IO ()
playScale tone scaleType duration pitch =
	play (bindNotes $ scaleNotes duration $ scalePitched pitch (scale scaleType tone))

playScaleWithDrums :: PitchClass -> ScaleType -> Dur -> Octave -> IO ()
playScaleWithDrums tone scaleType duration pitch = 
	play ((bindNotes $ scaleNotes duration $ scalePitched pitch (scale scaleType tone)) :=: (times 2 $ drumPhrase duration))

playScaleWithChord :: PitchClass -> ScaleType -> Dur -> Octave -> IO ()
playScaleWithChord tone scaleType duration pitch =
	play ((bindNotes $ scaleNotes duration $ scalePitched pitch (scale scaleType tone)) :=: (times 3 $ buildChord scaleType tone wn pitch))