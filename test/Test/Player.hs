module Test.Player where

import Kairos.Clock (Beats, TimeSignature (TS, beatInMsr, bpm, startTime))
import Kairos.Player (nextScheduledBeat, resolveScheduledBeat)
import Kairos.TimePoint (TimePoint, TimePointf (TP), j16, jGhost, nextBeat, toTP)
import Test.Hspec (describe, hspec, it, shouldBe, shouldSatisfy)

simulatePattern :: TimeSignature -> Beats -> [TimePoint] -> Int -> [Beats]
simulatePattern _ _ [] _ = []
simulatePattern ts startBeat pat@(firstTP : _) n =
  take n $ go firstTP (resolveScheduledBeat ts startBeat Nothing firstTP)
  where
    go tp target =
      let nb = nextBeat tp pat
          nextTarget = nextScheduledBeat ts target tp nb
       in target : go nb nextTarget

approxListEq :: [Beats] -> [Beats] -> Bool
approxListEq xs ys =
  length xs == length ys && and (zipWith (\a b -> abs (a - b) < 1e-12) xs ys)

playerTest :: IO ()
playerTest = hspec $ do
  let ts4 = TS {beatInMsr = 4, bpm = 120, startTime = 0}
      ts3 = TS {beatInMsr = 3, bpm = 120, startTime = 0}

  describe "nextScheduledBeat and multi-bar wrapping" $ do
    it "plays all four beats of every 4/4 bar for [0, 1, 2, 3]" $ do
      simulatePattern ts4 0.0 (toTP [0, 1, 2, 3]) 9
        `shouldBe` [0.0, 0.25, 0.5, 0.75, 1.0, 1.25, 1.5, 1.75, 2.0]

    it "crosses bar boundaries and respects trailing empty beats for [0, 2, 4, 5]" $ do
      simulatePattern ts4 0.0 (toTP [0, 2, 4, 5]) 9
        `shouldBe` [0.0, 0.5, 1.0, 1.25, 2.0, 2.5, 3.0, 3.25, 4.0]

    it "loops single-beat patterns [0] once per bar" $ do
      simulatePattern ts4 0.0 (toTP [0]) 4
        `shouldBe` [0.0, 1.0, 2.0, 3.0]

    it "handles jumps spanning multiple bars like [0, 9]" $ do
      simulatePattern ts4 0.0 (toTP [0, 9]) 5
        `shouldBe` [0.0, 2.25, 3.0, 5.25, 6.0]

    it "preserves leading empty bars like [4, 5]" $ do
      simulatePattern ts4 0.0 (toTP [4, 5]) 5
        `shouldBe` [1.0, 1.25, 3.0, 3.25, 5.0]

    it "schedules built-in multi-bar pattern jGhost without dropping bar-1 notes" $ do
      simulatePattern ts4 0.0 jGhost 6
        `shouldBe` [0.4375, 0.5625, 1.4375, 2.4375, 2.5625, 3.4375]

    it "schedules 4-bar pattern j16 accurately across all four bars" $ do
      simulatePattern ts4 0.0 j16 17
        `shouldBe` map (/ 4.0) [0, 1.75, 2.5, 3.5, 4, 5.75, 6.25, 7.5, 8, 9.75, 10, 11.5, 12, 13.75, 14.25, 15.5, 16]

    it "wraps multi-bar patterns correctly in 3/4 time" $ do
      simulatePattern ts3 0.0 (toTP [0, 2, 3, 4]) 9
        `shouldSatisfy` approxListEq (map (/ 3.0) [0, 2, 3, 4, 6, 8, 9, 10, 12])

  describe "resolveScheduledBeat across bar boundaries and live pattern switches" $ do
    it "keeps the exact scheduled beat on an early wakeup before a bar downbeat" $ do
      resolveScheduledBeat ts4 10.9999 (Just (TP 0, 11.0)) (TP 0)
        `shouldBe` 11.0

    it "anchors to the scheduled bar if toPlay changes externally during an early wakeup" $ do
      resolveScheduledBeat ts4 10.9999 (Just (TP 0, 11.0)) (TP 1)
        `shouldBe` 11.25

    it "re-anchors cleanly when switching from a multi-bar beat to a single-bar beat" $ do
      resolveScheduledBeat ts4 11.25 (Just (TP 5, 11.25)) (TP 3)
        `shouldBe` 11.75

    it "does not double-count the bar offset when switching between beats in bar 1" $ do
      resolveScheduledBeat ts4 11.375 (Just (TP 5.5, 11.375)) (TP 4)
        `shouldBe` 11.0

    it "advances bars when switching from bar 0 into a leading-silent-bar pattern" $ do
      resolveScheduledBeat ts4 10.75 (Just (TP 3, 10.75)) (TP 4)
        `shouldBe` 11.0
