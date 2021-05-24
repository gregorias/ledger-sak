module Test.LedgerDiff (
  tests,
) where

import LedgerDiff (diffLedgerText)
import NeatInterpolation (trimming)
import Relude
import Test.Hspec (SpecWith, describe, it)
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: SpecWith ()
tests = do
  describe "Test.LedgerDiff" $ do
    describe "diffLedgerText" $ do
      it "Outputs an empty diff on equal strings" $ do
        diffLedgerText "a" "a" `shouldBe` Right ""

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
        diffLedgerText original new `shouldBe` Right edDiff

      -- A different result would have been preferable but Neovim is stubborn
      -- about its inputs: https://github.com/neovim/neovim/issues/14522
      it "Outputs a correctly formatted ed-style diff" $ do
        let orig =
              [trimming|; preamble
                        ; orig

                        P 2021/04/01 1.00 CHF USD
                        P 2021/04/03 1.00 CHF USD
                        |]
            dest =
              [trimming|; preamble
                        ; dest

                        P 2021/04/02 1.00 CHF USD|]
            edDiff =
              [trimming|2c2
                        < ; orig
                        ---
                        > ; dest
                        4,5c4
                        < P 2021/04/01 1.00 CHF USD
                        < P 2021/04/03 1.00 CHF USD
                        ---
                        > P 2021/04/02 1.00 CHF USD
                        |]
        diffLedgerText orig dest `shouldBe` Right edDiff

      it "Uses empty lines as buffers nr 1" $ do
        let orig =
              [trimming|
                        2021-05-09 Coop L
                            content CC

                        2021-05-14 Coop L
                            content CC
                        |]
            dest =
              [trimming|
                        2021-05-10 * Coop R
                            content BCGE

                        2021/05/14 Coop R
                            content CC
                        |]
            edDiff =
              [trimming|
                        1,2c1,2
                        < 2021-05-09 Coop L
                        <     content CC
                        ---
                        > 2021-05-10 * Coop R
                        >     content BCGE
                        4c4
                        < 2021-05-14 Coop L
                        ---
                        > 2021/05/14 Coop R
                        |]
        diffLedgerText orig dest `shouldBe` Right edDiff
      it "Uses empty lines as buffers nr 2" $ do
        let orig =
              [trimming|
                        P 2021/04/01 1.00 CHF USD

                        P 2021/04/03 1.00 CHF USD
                        |]
            space = " "
            dest =
              [trimming|
                        P 2021/04/01 1.00 CHF USD

                        P 2021/04/02 1.00 CHF USD

                        P 2021/04/04 1.00 CHF USD
                        |]
            edDiff =
              [trimming|
                        1a2,3
                        >$space
                        > P 2021/04/02 1.00 CHF USD
                        3c5
                        < P 2021/04/03 1.00 CHF USD
                        ---
                        > P 2021/04/04 1.00 CHF USD
                        |]
        diffLedgerText orig dest `shouldBe` Right edDiff
