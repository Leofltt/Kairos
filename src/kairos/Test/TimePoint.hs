module Kairos.Test.TimePoint where

import Test.Hspec ( hspec, describe, it, shouldBe )
import Kairos.TimePoint

timePointTest :: IO ()
timePointTest = hspec $ do
    describe "Conversion from and to TP" $ do
        it "a list of TimePoints from Doubles" $ do
            toTP [4,5,6] `shouldBe` [TP 4, TP 5, TP 6]
        it "a list of Doubles from TimePoints" $ do
            fromTP [TP 7, TP 8, TP 9] `shouldBe` [7,8,9]
    describe "Functions to create a list of TimePoints" $ do
        it "checking tuplesForBar" $ do
            tupleForBar 4 3 `shouldBe` [TP 0.3333333333333333, TP 1.6666666666666665, TP 3.0]
            tupleForBar 4 8 `shouldBe` toTP [0.125, 0.625 .. 3.625]
        it "checking euclidean rhythm (3,8), no rotation" $ do
            putStrLn " 1 0 0 1 0 0 1 0"
            euclid (3,8) 0 4 `shouldBe` toTP [0, 1.5, 3]
        it "checking euclidean rhythm (3,8), one rotation left" $ do 
            putStrLn " 0 0 1 0 0 1 0 1"
            euclid (3,8) 1 4 `shouldBe` toTP [1, 2.5, 3.5]
        it "checking euclidean rhythm (3,8), two rotations left" $ do
            putStrLn " 0 1 0 0 1 0 1 0"
            euclid (3,8) 2 4 `shouldBe` toTP [0.5, 2, 3]
        it "checking patternWithDensity" $ do
            d <- patternWithDensity 4 16 100
            d `shouldBe` tupleForBar 4 16
            d <- patternWithDensity 4 10 100
            d `shouldBe` tupleForBar 4 10
        it "checking shine" $ do
            shine 4 "****" `shouldBe` toTP [0,1,2,3]
            shine 4 "~*~*" `shouldBe` toTP [1,3]
            shine 4 "~*~~~*~~" `shouldBe` toTP [0.5,2.5]
            shine 4 "*~~~~~*~~*~~" `shouldBe` toTP [0,2,3]
        -- todo : implement tests for other functions