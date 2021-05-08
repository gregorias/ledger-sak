module Test.LedgerDiff (
  tests,
) where

import LedgerDiff (diff)
import NeatInterpolation (trimming)
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: SpecWith ()
tests = do
  describe "Test.LedgerDiff" $ do
    describe "diff" $ do
      it "Outputs an empty diff on equal strings" $ do
        diff "a" "a" `shouldBe` ""
      it "Outputs a correctly formatted ed-style diff" $ do
        let original =
              [trimming|; aaa
                        ; ccc
                        ; ddd
                        ; 111
                        ; eee
                        ; fff
                        ; GGG|]
            new =
              [trimming|; 000
                        ; 111
                        ; aaa
                        ; bbb
                        ; ccc
                        ; ddd
                        ; eee
                        ; fff
                        ; ggg|]
            edDiff =
              [trimming|0a1,2
                        > ; 000
                        > ; 111
                        1a4
                        > ; bbb
                        4d6
                        < ; 111
                        7c9
                        < ; GGG
                        ---
                        > ; ggg|]
        diff original new `shouldBe` edDiff