module Test.Kit where

import Kairos.Kit
import Kairos.Pfield (Pfield (Ps))
import Test.Hspec (describe, hspec, it, shouldBe)

testKit :: Kit
testKit = newKit [(1, Ps "snare.wav"), (2, Ps "hihat.wav"), (3, Ps "kick.wav")]

kitTest :: IO ()
kitTest = hspec $ do
  describe "test kit creation" $ do
    it "should create a kit" $ do
      let kit = newKit [(1, Ps "snare.wav"), (2, Ps "hihat.wav"), (3, Ps "kick.wav")]
      kit `shouldBe` testKit
  describe "test conversion from keys to values" $ do
    it "should convert a list of keys from a list of valuse given a kit" $ do
      let list = fk testKit [1, 2, 3, 1]
      list `shouldBe` [Ps "snare.wav", Ps "hihat.wav", Ps "kick.wav", Ps "snare.wav"]
