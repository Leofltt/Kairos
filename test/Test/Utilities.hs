module Test.Utilities where

import Kairos.Utilities (genSeqU)
import Test.Hspec (describe, hspec, it, shouldBe)

-- A simple pure-like updater for testing
plusOneUpdater :: [Int] -> IO [Int]
plusOneUpdater (x:xs) = return (x + 1 : xs)
plusOneUpdater [] = return []

-- An updater that cycles through a list
cycleUpdater :: [Int] -> IO [Int]
cycleUpdater (x:xs) = return (xs ++ [x])
cycleUpdater [] = return []

utilitiesTest :: IO ()
utilitiesTest = hspec $ do
  describe "genSeqU" $ do
    it "should generate a sequence of the given length with a simple updater" $ do
      res <- genSeqU 5 plusOneUpdater [1]
      res `shouldBe` [2, 3, 4, 5, 6]

    it "should generate a sequence by cycling through a list" $ do
      res <- genSeqU 6 cycleUpdater [1, 2, 3]
      res `shouldBe` [2, 3, 1, 2, 3, 1]

    it "should return an empty list if n is 0" $ do
      res <- genSeqU 0 plusOneUpdater [1]
      res `shouldBe` []

    it "should stop if the updater returns an empty list" $ do
      let stopUpdater _ = return []
      res <- genSeqU 5 stopUpdater [1]
      res `shouldBe` []
