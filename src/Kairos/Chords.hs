module Kairos.Chords where

import Kairos.Utilities (offset)

type Chord = [Double]

-- | Create a list of pitch values for a chord given a root note
withChord :: Double -> Chord -> [Double]
root `withChord` chord = offset root (0.0 : chord)

-- | Helper to append an ensemble voice level (gain 0.0..1.0) to a 3-interval chord
--   resulting in [semitone2, semitone3, semitone4, level] for swaveEnsemble
withGain :: Double -> Chord -> [Double]
withGain gain [s2, s3, s4] = [s2, s3, s4, gain]
withGain _ other = other

-- Chords represented as intervals relative to the root note (excluding root 0.0)
-- Specifically suited for 4-voice synths like Swave Ensemble mode [semitone2, semitone3, semitone4]

-- Triads (doubling octave 12.0 for 4th voice)
maj :: Chord
maj = [4.0, 7.0, 12.0]

min' :: Chord
min' = [3.0, 7.0, 12.0]

sus2 :: Chord
sus2 = [2.0, 7.0, 12.0]

sus4 :: Chord
sus4 = [5.0, 7.0, 12.0]

dim :: Chord
dim = [3.0, 6.0, 12.0]

aug :: Chord
aug = [4.0, 8.0, 12.0]

-- 7th Chords
maj7 :: Chord
maj7 = [4.0, 7.0, 11.0]

min7 :: Chord
min7 = [3.0, 7.0, 10.0]

dom7 :: Chord
dom7 = [4.0, 7.0, 10.0]

dim7 :: Chord
dim7 = [3.0, 6.0, 9.0]

halfDim7 :: Chord
halfDim7 = [3.0, 6.0, 10.0]

minMaj7 :: Chord
minMaj7 = [3.0, 7.0, 11.0]

aug7 :: Chord
aug7 = [4.0, 8.0, 10.0]

augMaj7 :: Chord
augMaj7 = [4.0, 8.0, 11.0]

-- 6th Chords
maj6 :: Chord
maj6 = [4.0, 7.0, 9.0]

min6 :: Chord
min6 = [3.0, 7.0, 9.0]

-- Suspended 7th Chords
dom7sus4 :: Chord
dom7sus4 = [5.0, 7.0, 10.0]

dom7sus2 :: Chord
dom7sus2 = [2.0, 7.0, 10.0]

-- Extended / Jazz Voicings (rootless or 4-voice reductions)
-- 9th chords without 5th
maj9no5 :: Chord
maj9no5 = [4.0, 11.0, 14.0]

min9no5 :: Chord
min9no5 = [3.0, 10.0, 14.0]

dom9no5 :: Chord
dom9no5 = [4.0, 10.0, 14.0]

-- Add9
add9 :: Chord
add9 = [4.0, 7.0, 14.0]

minAdd9 :: Chord
minAdd9 = [3.0, 7.0, 14.0]

-- Altered Dominants
dom7b5 :: Chord
dom7b5 = [4.0, 6.0, 10.0]

dom7sharp5 :: Chord
dom7sharp5 = [4.0, 8.0, 10.0]

dom7b9 :: Chord
dom7b9 = [4.0, 10.0, 13.0]

dom7sharp9 :: Chord
dom7sharp9 = [4.0, 10.0, 15.0]

-- Open / Spread Voicings
openMin7 :: Chord
openMin7 = [7.0, 15.0, 22.0]

openMaj7 :: Chord
openMaj7 = [7.0, 16.0, 23.0]

-- Power Chord / Octaves
fifthOct :: Chord
fifthOct = [7.0, 12.0, 19.0]

octaves :: Chord
octaves = [12.0, 24.0, 36.0]
