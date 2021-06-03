{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.LedgerDiffQC (
  tests,
) where

import Control.Lens as L hiding (both)
import Data.Algorithm.Diff (
  Diff,
  PolyDiff (..),
 )
import LedgerDiff (
  SmartPiece (..),
  SmartPieceStatus (..),
  both,
  matchFillerRanges,
 )
import Relude hiding (First)
import Test.Hspec (
  Spec,
  describe,
 )
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck as QC (Arbitrary (..), elements)
import Test.QuickCheck.Property (withMaxSuccess)

data SmartPieceImpl
  = SpiFiller !Text
  | SpiContent !Text
  deriving stock (Show)

instance SmartPiece SmartPieceImpl where
  type SmartPieceContent SmartPieceImpl = Text

  getSpContent (SpiFiller t) = t
  getSpContent (SpiContent t) = t
  getSpStatus (SpiFiller _) = Filler
  getSpStatus (SpiContent _) = Meat

diffToTwoFiles :: [Diff p] -> ([p], [p])
diffToTwoFiles [] = ([], [])
diffToTwoFiles (First f : dps) = L.over _1 (f :) $ diffToTwoFiles dps
diffToTwoFiles (Second s : dps) = L.over _2 (s :) $ diffToTwoFiles dps
diffToTwoFiles (Both l r : dps) = bimap (l :) (r :) $ diffToTwoFiles dps

prop_matchFillerRanges_preservesContent :: [Diff SmartPieceImpl] -> Bool
prop_matchFillerRanges_preservesContent dss =
  original == diffToTwoFiles (matchFillerRanges dss)
 where
  originalSpi :: ([SmartPieceImpl], [SmartPieceImpl]) = diffToTwoFiles dss
  original :: ([Text], [Text]) = both (map getSpContent) originalSpi

instance Arbitrary SmartPieceImpl where
  arbitrary = do
    spi :: (Text -> SmartPieceImpl) <- (\x -> if x then SpiFiller else SpiContent) <$> arbitrary
    content :: [Char] <- one <$> arbitrary
    return $ spi $ toText content

instance Arbitrary a => Arbitrary (PolyDiff a a) where
  arbitrary = do
    l <- arbitrary
    r <- arbitrary
    QC.elements [First l, Second r, Both l r]

tests :: Spec
tests = do
  describe "Test.LedgerDiffQC" $ do
    describe "matchFillerRanges" $ do
      prop "preservesContent" (withMaxSuccess 10000 prop_matchFillerRanges_preservesContent)
