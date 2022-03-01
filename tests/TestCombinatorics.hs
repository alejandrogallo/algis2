{-# LANGUAGE UnicodeSyntax
  #-}
module TestCombinatorics where

import Test.Hspec

import Algis

testCombinatorics ∷ Spec
testCombinatorics = do
    describe "getSubsequences" $ do
      it "should create correct for abcd" $ do
        getSubsequences [2, 2] "abcd" `shouldBe` [ ["ab","cd"]
                                                 , ["ac","bd"]
                                                 , ["ad","bc"]
                                                 ]
