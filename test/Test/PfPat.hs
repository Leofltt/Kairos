module Test.PfPat where

import Test.Hspec ( hspec, describe, it, shouldBe )
import Kairos.PfPat
import Kairos.Pfield ( Pfield(Pd), new_pfId )
import Control.Concurrent.STM (newTVarIO, TVar)

testPat :: IO (TVar [Pfield])
testPat = newTVarIO [Pd 1,Pd 2,Pd 3]

testAPat :: IO (TVar [Pfield])
testAPat = newTVarIO [Pd 0]

testPfPat :: IO PfPat
testPfPat = do
    pfP <- testPat
    return $ PfPat { pat = pfP, pfId = new_pfId 1 "testId", updater = keep }

testPfPatA :: IO PfPat
testPfPatA = do
    pfP <- testAPat
    return $ PfPat { pat = pfP, pfId = new_pfId 1 "testId", updater = keep }

pfPatTest :: IO ()
pfPatTest = hspec $ do
    describe "test keep updater" $ do
        it "should return the head of the pat" $ do
            testPfP <- testPfPat
            newP <- keep testPfP
            newP `shouldBe` Pd 1
    describe "test nextVal updater" $ do
        it "should return the next value of the pat" $ do
            testPfP <- testPfPat
            newP <- nextVal testPfP >> nextVal testPfP
            newP `shouldBe` Pd 2
    describe "test retrograde updater" $ do
        it "should return the last value of the pat" $ do
            testPfP <- testPfPat
            newP <- retrograde testPfP >> retrograde testPfP
            newP `shouldBe` Pd 3
    describe "test percentNext updater" $ do
        it "should return the next value of the pat" $ do
            testPfP <- testPfPat
            newP <- percentNext 100 testPfP >> percentNext 100 testPfP
            newP `shouldBe` Pd 2
        it "should return the head of the pat" $ do
            testPfP <- testPfPat
            newP <- percentNext 0 testPfP >> percentNext 0 testPfP
            newP `shouldBe` Pd 1
    describe "test auto updater" $ do
        it "should keep the same value if pat is 1 element" $ do
            testPfP <- testPfPatA
            newP <- a testPfP
            newP `shouldBe` Pd 0
        it "should return the next value of the pat if pat is longer than 1 element" $ do
            testPfP <- testPfPat
            newP <- a testPfP >> a testPfP
            newP `shouldBe` Pd 2