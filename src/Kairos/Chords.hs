module Kairos.Chords where

import Kairos.Utilities (offset)

type Chord = [Double]

-- | Create a list of pitch values for a chord given a root note
--   Identical in convention to withScale (e.g. 60 `withChord` min7)
withChord :: Double -> Chord -> [Double]
root `withChord` chord = offset root chord

-- | Helper to append an ensemble voice level (gain 0.0..1.0) to a chord
withGain :: Double -> Chord -> [Double]
withGain gain chord = chordToEnsemble chord ++ [gain]

-- | Adapter to convert a standard root-based chord [0.0, s2, s3, s4] (or arbitrary chord)
--   into the 3 upper interval offsets expected by Aillen swave ensemble [s2, s3, s4].
chordToEnsemble :: Chord -> [Double]
chordToEnsemble (0.0 : s2 : s3 : s4 : _) = [s2, s3, s4]
chordToEnsemble [s2, s3, s4]             = [s2, s3, s4] 
chordToEnsemble [s2, s3]                 = [s2, s3, 12.0] -- 3-note triad: double the octave
chordToEnsemble other                    = other

-- | Normalize chord or interval list for swaveEnsemble:
--   - If given [0.0, s2, s3, s4] (standard 4-voice Chord), converts to [s2, s3, s4, 0.8]
--   - If given [0.0, s2, s3] (standard triad), doubles octave [s2, s3, 12.0, 0.8]
--   - If given [s2, s3, s4] (3 intervals), appends default level 0.8 -> [s2, s3, s4, 0.8]
--   - If given [s2, s3, s4, level] (already formatted), preserves as-is
normalizeEnsemble :: [Double] -> [Double]
normalizeEnsemble (0.0 : s2 : s3 : s4 : level : _) = [s2, s3, s4, level]
normalizeEnsemble (0.0 : s2 : s3 : s4 : _)        = [s2, s3, s4, 0.8]
normalizeEnsemble (0.0 : s2 : s3 : _)             = [s2, s3, 12.0, 0.8]
normalizeEnsemble [s2, s3, s4]                    = [s2, s3, s4, 0.8]
normalizeEnsemble other                           = other

-- =========================================================================
-- Standard Chords (all start with 0.0 root, just like Scale in Kairos.Scales)
-- =========================================================================

-- Triads
maj :: Chord
maj = [0.0, 4.0, 7.0]

min' :: Chord
min' = [0.0, 3.0, 7.0]

sus2 :: Chord
sus2 = [0.0, 2.0, 7.0]

sus4 :: Chord
sus4 = [0.0, 5.0, 7.0]

dim :: Chord
dim = [0.0, 3.0, 6.0]

aug :: Chord
aug = [0.0, 4.0, 8.0]

-- 7th Chords
maj7 :: Chord
maj7 = [0.0, 4.0, 7.0, 11.0]

min7 :: Chord
min7 = [0.0, 3.0, 7.0, 10.0]

dom7 :: Chord
dom7 = [0.0, 4.0, 7.0, 10.0]

dim7 :: Chord
dim7 = [0.0, 3.0, 6.0, 9.0]

halfDim7 :: Chord
halfDim7 = [0.0, 3.0, 6.0, 10.0]

minMaj7 :: Chord
minMaj7 = [0.0, 3.0, 7.0, 11.0]

aug7 :: Chord
aug7 = [0.0, 4.0, 8.0, 10.0]

augMaj7 :: Chord
augMaj7 = [0.0, 4.0, 8.0, 11.0]

-- 6th Chords
maj6 :: Chord
maj6 = [0.0, 4.0, 7.0, 9.0]

min6 :: Chord
min6 = [0.0, 3.0, 7.0, 9.0]

-- Suspended 7th Chords
dom7sus4 :: Chord
dom7sus4 = [0.0, 5.0, 7.0, 10.0]

dom7sus2 :: Chord
dom7sus2 = [0.0, 2.0, 7.0, 10.0]

-- Extended / 9th Chords
maj9no5 :: Chord
maj9no5 = [0.0, 4.0, 11.0, 14.0]

min9no5 :: Chord
min9no5 = [0.0, 3.0, 10.0, 14.0]

dom9no5 :: Chord
dom9no5 = [0.0, 4.0, 10.0, 14.0]

add9 :: Chord
add9 = [0.0, 4.0, 7.0, 14.0]

minAdd9 :: Chord
minAdd9 = [0.0, 3.0, 7.0, 14.0]

-- Altered Dominants
dom7b5 :: Chord
dom7b5 = [0.0, 4.0, 6.0, 10.0]

dom7sharp5 :: Chord
dom7sharp5 = [0.0, 4.0, 8.0, 10.0]

dom7b9 :: Chord
dom7b9 = [0.0, 4.0, 10.0, 13.0]

dom7sharp9 :: Chord
dom7sharp9 = [0.0, 4.0, 10.0, 15.0]

-- Open / Spread Voicings
openMin7 :: Chord
openMin7 = [0.0, 7.0, 15.0, 22.0]

openMaj7 :: Chord
openMaj7 = [0.0, 7.0, 16.0, 23.0]

-- Power Chords & Octaves
fifthOct :: Chord
fifthOct = [0.0, 7.0, 12.0, 19.0]

octaves :: Chord
octaves = [0.0, 12.0, 24.0, 36.0]
