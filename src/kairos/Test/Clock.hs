module Kairos.Test.Clock where

import Test.Hspec ( hspec, describe, it, shouldBe )
import Kairos.Clock
import Control.Concurrent.STM ( readTVarIO )

clockTest :: IO ()
clockTest = hspec $ do
    describe "Time Signature test" $ do
        it "should be able to create a time signature" $ do
            newTS 120 4 0 `shouldBe` TS { beatInMsr = 4, bpm = 120, startTime = 0 }
        it "should be able to change time signature" $ do
            c <- defaultClock 
            let ts = newTS 133 5 0
            ts' <- addTS c ts 
            show (bpm $ head ts') `shouldBe` "133.0"
            show (beatInMsr $ head ts') `shouldBe` "5.0"
        it "should be able to change bpm (aka create a newTS that's the same, but with different bpm)" $ do
            c <- defaultClock 
            changeTempo c 140
            ts <- readTVarIO $ timeSig c
            show (bpm $ head ts) `shouldBe` "140.0"
            show (bpm $ last ts) `shouldBe` "120.0"
        -- todo : implement tests for other functions