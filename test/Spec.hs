import Test.HUnit
import Euterpea
import MusicBuilder

main :: IO ()
main = do
                runTestTT tests
                return()

tests = TestList [TestLabel "Major scale construction test" majorScaleConstruction,
                  TestLabel "Minor scale construction test" minorScaleConstruction,
                  TestLabel "Scale pitching test" scalePitching,
                  TestLabel "Major chord construction test" majorChordConstruction,
                  TestLabel "Minor chord construction test" minorChordConstruction]

majorScaleConstruction = TestCase (
        do
                assertEqual "For C Major Scale" [C,D,E,F,G,A,B] (majorScale C)
                assertEqual "For E Major Scale" [E,Fs,Gs,A,B,Cs,Ds] (majorScale E)
        )

minorScaleConstruction = TestCase (
        do
                assertEqual "For C Minor Scale" [C,D,Ds,F,G,Gs,As] (minorScale C)
                assertEqual "For E Minor Scale" [E,Fs,G,A,B,C,D] (minorScale E)
        )
        
scalePitching = TestCase (
        do
                assertEqual "For C Major Scale" [(C,4),(D,4),(E,4),(F,4),(G,4),(A,4),(B,4)] (scalePitched 4 (majorScale C))
                assertEqual "For E Minor Scale" [(E,4),(Fs,4),(G,4),(A,4),(B,4),(C,5),(D,5)] (scalePitched 4 (minorScale E))
        )
        
majorChordConstruction = TestCase (
        do     
                assertEqual "For A Major Chord" [A,Cs,E] (majorChord A)
                assertEqual "For B Major Chord" [B,Ds,Fs] (majorChord B)
        )
        
minorChordConstruction = TestCase (
        do
                assertEqual "For A Minor Chord" [A,C,E] (minorChord A)
                assertEqual "For B Minor Chord" [B,D,Fs] (minorChord B)
        )
        

