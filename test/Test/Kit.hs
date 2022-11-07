module Test.Kit where

import Test.Hspec ( hspec, describe, it, shouldBe )
import Kairos.Kit
import Kairos.Pfield(Pfield (Ps))

testKit :: Kit 
testKit = newKit [("snare", Ps "snare.wav"), ("hihat", Ps "hihat.wav"), ("kick", Ps "kick.wav")]

kitTest :: IO ()
kitTest = hspec $ do
    describe "test kit creation" $ do
        it "should create a kit" $ do
            let kit = newKit [("snare", Ps "snare.wav"), ("hihat", Ps "hihat.wav"), ("kick", Ps "kick.wav")]
            kit `shouldBe` testKit
    describe "test conversion from keys to values" $ do
        it "should convert a list of keys from a list of valuse given a kit" $ do 
            let list = fk testKit ["snare", "hihat", "kick", "snare"]
            list `shouldBe` [Ps "snare.wav", Ps "hihat.wav", Ps "kick.wav", Ps "snare.wav"]    
