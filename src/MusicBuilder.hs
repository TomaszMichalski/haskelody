module MusicBuilder where

import Euterpea
import Data.List
import Data.Ratio


bassDrum = perc AcousticBassDrum
snareDrum = perc AcousticSnare
hiHat = perc ClosedHiHat

-- drum phrase definition

{- |
Implementacja prostych perkusyjnych œcie¿ek.
-}
drumPhrase note = b :=: h :+: h :+: s :=: h :+: h
        where b = bassDrum note
              s = snareDrum note
              h = hiHat note
              
{- |
Lista wszystkich dŸwiêków
-}
notes :: [PitchClass]
notes = [C, Cs, D, Ds, E, F, Fs, G, Gs, A, As, B]


{- |
Definicja sposobu tworzenia skali durowej.
-}
majorScalePattern = [True, False, True, False, True, True, False, True, False, True, False, True]

{- |
Definicja sposobu tworzernia akordu Dur.
-}
minorChordPattern = [True, False, False, True, False, False, False, True]


{- |
Definicja sposobu tworzenia skali skali Mol.
-}
minorScalePattern = [True, False, True, True, False, True, False, True, True, False, True, False]


{- |
Definicja sposobu akordu Dur.
-}
majorChordPattern = [True, False, False, False, True, False, False, True]

type PureNote = Octave -> Dur -> Music Pitch


{- |
Funkcja filtruj¹ca dŸwiêki.
-}
dropNotes :: (Integral a) => Maybe a -> [PitchClass] -> [PitchClass]
dropNotes Nothing xs = xs
dropNotes (Just 0) xs = xs
dropNotes (Just n) [] = []
dropNotes (Just n) (x:xs) = dropNotes (Just (n-1)) xs

{- |
Funkcja filtruj¹ca dŸwiêki.
-}
takeNotes :: (Integral a) => Maybe a -> [PitchClass] -> [PitchClass]    
takeNotes Nothing xs = xs
takeNotes (Just 0) _ = []
takeNotes (Just n) [] = []
takeNotes (Just n) (x:xs) = x : (takeNotes (Just (n-1)) xs)

{- |
Funkcja tworz¹ca skalê durow¹ jako listê nazw dŸwiêków
-}
majorScale :: PitchClass -> [PitchClass]
majorScale note = map fst . filter (\(_,y) -> y == True) $ zip notesCycled majorScalePattern
        where noteIndex = note `elemIndex` notes
              notesCycled = dropNotes noteIndex (cycle notes)
{- |
Funkcja tworz¹ca akord Durowy jako listê nazw dŸwiêków
-} 			  
majorChord :: PitchClass -> [PitchClass]
majorChord note = map fst . filter (\(_,y) -> y == True) $ zip notesCycled majorChordPattern
        where noteIndex = note `elemIndex` notes
              notesCycled = dropNotes noteIndex (cycle notes)
			  
{- |
Funkcja tworz¹ca skalê molow¹ jako listê nazw dŸwiêków
-}            
minorScale :: PitchClass -> [PitchClass]
minorScale note = map fst . filter (\(_,y) -> y == True) $ zip notesCycled minorScalePattern
        where noteIndex = note `elemIndex` notes
              notesCycled = dropNotes noteIndex (cycle notes)
{- |
Funkcja tworz¹ca akord Molowy jako listê nazw dŸwiêków
-} 			  
minorChord :: PitchClass -> [PitchClass]
minorChord note = map fst . filter (\(_,y) -> y == True) $ zip notesCycled minorChordPattern
        where noteIndex = note `elemIndex` notes
              notesCycled = dropNotes noteIndex (cycle notes)

{- |
Funkcja dodaj¹ca do listy dŸwiêków oktawy dla ka¿dego dŸwiêku.
-} 			  			  
scalePitched :: Octave -> [PitchClass] -> [Pitch]
scalePitched _ [] = []
scalePitched octave (x:[]) = [(x,octave)]
scalePitched octave (x:xs) = [(x,octave)] ++ (if(head xs < x) then (scalePitched (octave+1) xs) else (scalePitched octave xs))

{- |
Funkcja usuwaj¹ca oktawê z listy nut
-}
dropPitch :: [Pitch] -> [PitchClass]
dropPitch xs = [ x | (x,octave) <- xs]


scalePitchedN :: Int -> [PitchClass] -> [Pitch]
scalePitchedN n notes
        | n <= 0 = []
        | otherwise = (scalePitchedN (n-1) notes) ++ (scalePitched n notes)


{- |
Funkcja dodaj¹ca do listy dŸwiêków d³ugosæ dla ka¿dego dŸwiêku.
-} 			  		
scaleNotes :: Dur -> [Pitch] -> [Music Pitch]
scaleNotes duration notes = [note duration n | n <- notes]


{- |
Funkcja scalaj¹ca dŸwiêki do formatu doostêpnego dla odtwarzacza z biblioteki Euterpea.
-} 			  		
bindNotes :: [Music Pitch] -> Music Pitch
bindNotes (x:[]) = x
bindNotes (x:xs) = x :+: (bindNotes xs)

bindChord :: [Music Pitch] -> Music Pitch
bindChord (x:[]) = x
bindChord (x:xs) = x :=: (bindChord xs)


{- |
Typ danych definiuj¹cy typy skal
-} 			  		
data ScaleType = Min | Maj


{- |
Funkcja tworz¹ca skale podanego typu jako listy dŸwieków.
-} 			  		
scale :: ScaleType -> PitchClass -> [PitchClass]
scale Min ton = minorScale(ton)
scale Maj ton = majorScale(ton)


{- |
Funkcja buduj¹ca akord dla podanych atrybutów.
-}
buildChord :: ScaleType -> PitchClass -> Dur -> Octave -> Music Pitch
buildChord Min ton duration pit = bindChord $ scaleNotes duration $ scalePitched pit (minorChord ton)
buildChord Maj ton duration pit = bindChord $ scaleNotes duration $ scalePitched pit (majorChord ton)