module Test.LedgerDiff (
  tests,
) where

import Data.Algorithm.Diff (
  PolyDiff (Both, First, Second),
 )
import LedgerDiff (
  SectionChunk (..),
  SmartPieceStatus (..),
  diffLedgerText,
  matchFillerRanges,
 )
import NeatInterpolation (trimming)
import Relude hiding (First)
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

      it "Uses empty lines as buffers inside dated chunks" $ do
        let orig =
              [trimming|
                        2021/04/01 firstl
                          contentfirst

                        2021/04/01 intermediaterubbish
                          contentrubbish

                        2021/04/01 secondl
                          contentsecond
                        |]
            dest =
              [trimming|
                        2021/04/01 firstr
                          contentfirst

                        2021/04/01 secondr
                          contentsecond
                        |]
            space = " "
            edDiff =
              [trimming|
                        1c1
                        < 2021/04/01 firstl
                        ---
                        > 2021/04/01 firstr
                        3,5d2
                        <$space
                        < 2021/04/01 intermediaterubbish
                        <   contentrubbish
                        7c4
                        < 2021/04/01 secondl
                        ---
                        > 2021/04/01 secondr
                        |]
        diffLedgerText orig dest `shouldBe` Right edDiff

      it "Correctly diffs a file 0" $ do
        let orig =
              [trimming|
                        2021-04-27 * AMZN Mktp DE, AMAZON.DE
                          Assets:Liquid:BCGE CC            CHF -64.60
                          Expenses:Financial Services        CHF 1.10
                          Expenses:B                       CHF  63.50
                        |]
            dest =
              [trimming|
                        2021-04-27 * Coop
                            Assets:Liquid:BCGE                 CHF -6.75
                            Expenses:Gesundheit                   CHF 5.15
                            Expenses:Groceries:C                  CHF 1.60

                        2021/04/27 * Amazon -- Stuff
                            Assets:Liquid:BCGE CC            CHF -64.60
                            Expenses:Financial Services        CHF 1.10
                            Expenses:Haushalt
                        |]
            space = " "
            edDiff =
              [trimming|
                        1c1,6
                        < 2021-04-27 * AMZN Mktp DE, AMAZON.DE
                        ---
                        > 2021-04-27 * Coop
                        >     Assets:Liquid:BCGE                 CHF -6.75
                        >     Expenses:Gesundheit                   CHF 5.15
                        >     Expenses:Groceries:C                  CHF 1.60
                        >$space
                        > 2021/04/27 * Amazon -- Stuff
                        4c9
                        <   Expenses:B                       CHF  63.50
                        ---
                        >     Expenses:Haushalt
                        |]
        diffLedgerText orig dest `shouldBe` Right edDiff

      it "Correctly diffs a file 1" $ do
        let orig =
              [trimming|
                        2021-04-23 * LAEDERACH CHOCOLAT.
                            Assets:Liquid:BCGE CC      CHF -39.90

                        2021-04-27 * AMZN Mktp DE, AMAZON.DE
                          Assets:Liquid:BCGE CC            CHF -64.60
                          Expenses:Financial Services        CHF 1.10
                          Expenses:B                       CHF  63.50
                        |]
            dest =
              [trimming|
                        2021-04-23 * LAEDERACH CHOCOLAT.
                            Assets:Liquid:BCGE CC      CHF -39.90

                        2021-04-23 * VT trade
                            Assets:Investments:IB:VT              VT 91

                        2021-04-27 * Coop
                            Assets:Liquid:BCGE                 CHF -6.75
                            Expenses:Gesundheit                   CHF 5.15
                            Expenses:Groceries:C                  CHF 1.60

                        2021/04/27 * Amazon -- Stuff
                            Assets:Liquid:BCGE CC            CHF -64.60
                            Expenses:Financial Services        CHF 1.10
                            Expenses:Haushalt
                        |]
            space = " "
            edDiff =
              [trimming|
                        2a3,10
                        >$space
                        > 2021-04-23 * VT trade
                        >     Assets:Investments:IB:VT              VT 91
                        >$space
                        > 2021-04-27 * Coop
                        >     Assets:Liquid:BCGE                 CHF -6.75
                        >     Expenses:Gesundheit                   CHF 5.15
                        >     Expenses:Groceries:C                  CHF 1.60
                        4c12
                        < 2021-04-27 * AMZN Mktp DE, AMAZON.DE
                        ---
                        > 2021/04/27 * Amazon -- Stuff
                        7c15
                        <   Expenses:B                       CHF  63.50
                        ---
                        >     Expenses:Haushalt
                        |]
        diffLedgerText orig dest `shouldBe` Right edDiff

    describe "matchFillerRanges" $ do
      it "matches 0" $ do
        let result =
              matchFillerRanges @(SmartPieceStatus, Text)
                [ First (Filler, "l0")
                , Second (Filler, "r0")
                , Second (Meat, "2")
                , Second (Filler, "r1")
                , First (Meat, "3")
                , Second (Meat, "4")
                ]
        result
          `shouldBe` [ Second "r0"
                     , Second "2"
                     , Both "l0" "r1"
                     , First "3"
                     , Second "4"
                     ]

      it "matches 1" $ do
        let result =
              matchFillerRanges @(SmartPieceStatus, Text)
                [ First (Meat, "9")
                , First (Filler, "l0")
                , Second (Meat, "10")
                , Second (Filler, "r0")
                , Both (Meat, "14") (Meat, "14")
                ]
        result
          `shouldBe` [First "9", Second "10", Both "l0" "r0", Both "14" "14"]

      it "matches 2" $ do
        let lscContent = "2021-04-23 * LAEDERACH CHOCOLAT.\n"
            lsc = DatedSectionChunk lscContent
            uc = UndatedSectionChunk "\n"
            vrContent = "2021-04-23 * Trade\n"
            vr = DatedSectionChunk vrContent
            diffs = [Both lsc lsc, Second uc, Second vr]
        matchFillerRanges diffs
          `shouldBe` [ Both lscContent lscContent
                     , Second "\n"
                     , Second vrContent
                     ]
