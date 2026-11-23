module Test.TimePoint where

import Kairos.TimePoint
import Test.Hspec (describe, hspec, it, shouldBe)

timePointTest :: IO ()
timePointTest = hspec $ do
  describe "Conversion from and to TP" $ do
    it "a list of TimePoints from Doubles" $ do
      toTP [4, 5, 6] `shouldBe` [TP 4, TP 5, TP 6]
    it "a list of Doubles from TimePoints" $ do
      fromTP [TP 7, TP 8, TP 9] `shouldBe` [7, 8, 9]
  describe "Check that sequencing TP Patterns works" $ do
    it "can calculate the next TP" $ do
      nextBeat (TP 1) [TP 0, TP 1, TP 2, TP 3] `shouldBe` TP 2
      nextBeat (TP 3) [TP 0, TP 1, TP 2, TP 3] `shouldBe` TP 0
  describe "Functions to create a list of TimePoints" $ do
    it "checking tuplesForBar" $ do
      tupleForBar 4 3 `shouldBe` [TP 0.3333333333333333, TP 1.6666666666666665, TP 3.0]
      tupleForBar 4 8 `shouldBe` toTP [0.125, 0.625 .. 3.625]
    it "checking euclidean rhythm (3,8), no rotation" $ do
      putStrLn " 1 0 0 1 0 0 1 0"
      euclid (3, 8) 0 4 `shouldBe` toTP [0, 1.5, 3]
    it "checking euclidean rhythm (3,8), one rotation right" $ do
      putStrLn " 0 1 0 0 1 0 0 1"
      euclid (3, 8) 1 4 `shouldBe` toTP [0.5, 2, 3.5]
    it "checking euclidean rhythm (3,8), two rotations left" $ do
      putStrLn " 0 1 0 0 1 0 1 0"
      euclid (3, 8) (-2) 4 `shouldBe` toTP [0.5, 2, 3]
    it "checking patternWithDensity" $ do
      d <- patternWithDensity 4 16 100
      d `shouldBe` tupleForBar 4 16
      pat <- patternWithDensity 4 10 100
      pat `shouldBe` tupleForBar 4 10
    it "checking shine" $ do
      shine 4 "****" `shouldBe` toTP [0, 1, 2, 3]
      shine 4 "~*~*" `shouldBe` toTP [1, 3]
      shine 4 "~*~~~*~~" `shouldBe` toTP [0.5, 2.5]
      shine 4 "*~~~~~*~~*~~" `shouldBe` toTP [0, 2, 3]
  describe "Concatenating and sequencing TP patterns" $ do
    it "catBar rounds up to the next bar boundary for single and multi-bar patterns" $ do
      catBar 4 (toTP [0, 2]) (toTP [0, 1]) `shouldBe` toTP [0, 2, 4, 5]
      catBar 4 (toTP [0, 1, 2, 3]) (toTP [0, 1]) `shouldBe` toTP [0, 1, 2, 3, 4, 5]
      catBar 4 (toTP [0, 2, 4, 5]) (toTP [0, 1]) `shouldBe` toTP [0, 2, 4, 5, 8, 9]
      catBar 3 (toTP [0, 2]) (toTP [0, 1]) `shouldBe` toTP [0, 2, 3, 4]
      catBar 4 [] (toTP [0, 1]) `shouldBe` toTP [4, 5]
    it "catBars chains multiple patterns and preserves silent middle bars" $ do
      catBars 4 [] `shouldBe` []
      catBars 4 [toTP [0, 2]] `shouldBe` toTP [0, 2]
      catBars 4 [toTP [0, 2], [], toTP [0, 1]] `shouldBe` toTP [0, 2, 8, 9]
      catBars 4 [toTP [0, 1, 2, 3], toTP [0, 2], toTP [1, 3]] `shouldBe` toTP [0, 1, 2, 3, 4, 6, 9, 11]
    it "catTP' defaults catBar to 4/4" $ do
      catTP' (toTP [0, 2]) (toTP [0, 1]) `shouldBe` toTP [0, 2, 4, 5]
    it "catBar 1 joins on the next whole beat without crashing on empty lists" $ do
      catBar 1 (toTP [0, 2]) (toTP [0, 1]) `shouldBe` toTP [0, 2, 3, 4]
      catBar 1 (toTP [0, 2.5]) (toTP [0, 1]) `shouldBe` toTP [0, 2.5, 3, 4]
      catBar 1 [] (toTP [0, 1]) `shouldBe` toTP [1, 2]

-- todo : implement tests for other functions