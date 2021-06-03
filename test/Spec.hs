module Spec (main) where

import Relude
import Test.Hspec (SpecWith, hspec)
import qualified Test.LedgerDiff
import qualified Test.LedgerDiffQC
import qualified Test.Parse

main :: IO ()
main = hspec tests

tests :: SpecWith ()
tests = do
  Test.LedgerDiff.tests
  Test.LedgerDiffQC.tests
  Test.Parse.tests
