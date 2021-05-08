module Test.Parse (
  tests,
) where

import Data.Time (fromGregorian)
import NeatInterpolation (trimming, untrimming)
import Parse (
  Chunk (..),
  DatedChunk (..),
  Journal (..),
  UndatedChunk (..),
  parseJournal,
 )
import Relude
import Test.Hspec (
  Expectation,
  SpecWith,
  describe,
  it,
 )
import Test.Hspec.Expectations.Pretty (shouldBe)

tests :: SpecWith ()
tests = do
  describe "Test.Parse" $ do
    describe "parseJournal" $ do
      it "parses a journal" shouldParseJournal

shouldParseJournal :: Expectation
shouldParseJournal =
  parseJournal journal
    `shouldBe` Right
      ( Journal
          [ ChunkUndatedChunk
              ( UndatedChunk
                  [untrimming|; -*- ledger -*-
                          ; comment

                          account Assets:Investments
                          note Liquid financial assets that appreciate in value.
                          |]
              )
          , ChunkDatedChunk
              ( DatedChunk
                  (fromGregorian 2019 4 22)
                  ([trimming|P 2019/04/22 CHF HRK 6.52|] <> "\n")
              )
          , ChunkUndatedChunk (UndatedChunk "\n")
          , ChunkDatedChunk
              ( DatedChunk
                  (fromGregorian 2020 1 1)
                  [trimming|
                  2020/01/01 * Patreon
                    Assets:Liquid:Revolut:EUR  EUR -8.05
                    Expenses:Leisure|]
              )
          ]
      )
 where
  journal =
    [trimming|; -*- ledger -*-
              ; comment

              account Assets:Investments
              note Liquid financial assets that appreciate in value.

              P 2019/04/22 CHF HRK 6.52

              2020/01/01 * Patreon
                Assets:Liquid:Revolut:EUR  EUR -8.05
                Expenses:Leisure|]
