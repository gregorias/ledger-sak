{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
-- For doctests
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Generates a smart diff of ledger files.
module LedgerDiff (
  diffLedgerText,
  diffLedgerIO,

  -- * Internal functions exposed for testing
  both,
  Section (..),
  SectionChunk (..),
  SmartPiece (..),
  SmartPieceStatus (..),
  matchFillerRanges,
  groupChunksIntoSections,
) where

import Data.Algorithm.Diff (
  Diff,
  PolyDiff (Both, First, Second),
  getDiffBy,
 )
import qualified Data.Text as T
import Data.Time (Day)
import Data.Time.Calendar (fromGregorian)
import Ed (EdHunk, edHunksToEdDiff, groupedDiffToEdHunks)
import Parse (
  Chunk (ChunkDatedChunk, ChunkUndatedChunk),
  DatedChunk (..),
  Journal (Journal),
  UndatedChunk (..),
  parseJournal,
 )
import Relude hiding (First)

-- | Apply a single function to both components of a pair.
--
-- >>> both succ (1,2)
-- (2,3)
both :: (a -> b) -> (a, a) -> (b, b)
both f (x, y) = (f x, f y)

-- | A function that encodes whether two Ledger lines are similar.
--
-- Two ledger lines are similar if they differ only in:
-- * Space chars
-- * The transaction or posting status
--
-- >>> similarLedgerLine "  * Bank   1 CHF" "  Bank  1 CHF"
-- True
similarLedgerLine :: Text -> Text -> Bool
similarLedgerLine l r = process l == process r
 where
  dropWhitespace = T.filter (/= ' ')
  dropStatus = T.dropWhile (\c -> c == '!' || c == '*')
  process = dropStatus . dropWhitespace

data SectionChunk
  = DatedSectionChunk Text
  | UndatedSectionChunk Text
  deriving stock (Eq, Show)

sectionChunkToText :: SectionChunk -> Text
sectionChunkToText (DatedSectionChunk content) = content
sectionChunkToText (UndatedSectionChunk content) = content

-- | A section is a grouping of chunks.
data Section
  = UndatedSection Text
  | DatedSection Day [SectionChunk]
  deriving stock (Eq, Show)

sectionToSectionChunks :: Section -> [SectionChunk]
sectionToSectionChunks (UndatedSection t) = one $ UndatedSectionChunk t
sectionToSectionChunks (DatedSection _ cs) = cs

-- | Groups together consecutive undated chunks and same-dated chunks.
--
-- Section here is a sequence of undated chunks or same-dated chunks
-- interspersed with undated chunks.
--
-- >>> :{
--   groupChunksIntoSections
--     [ ChunkDatedChunk (DatedChunk (fromGregorian 2021 5 30) "1")
--     , ChunkUndatedChunk (UndatedChunk "yo")
--     , ChunkUndatedChunk (UndatedChunk "lo")
--     , ChunkDatedChunk (DatedChunk (fromGregorian 2021 5 30) "2")]
-- :}
-- [DatedSection 2021-05-30 [DatedSectionChunk "1",UndatedSectionChunk "yolo",DatedSectionChunk "2"]]
groupChunksIntoSections :: [Chunk] -> [Section]
groupChunksIntoSections = groupDatedWithInterspersedUndated . groupUndated
 where
  groupUndated :: [Chunk] -> [Chunk]
  groupUndated [] = []
  groupUndated (ChunkUndatedChunk uc : (ChunkUndatedChunk uc' : cs)) =
    groupUndated (ChunkUndatedChunk (uc <> uc') : cs)
  groupUndated (x : xs) = x : groupUndated xs

  takeDate :: Day -> [Chunk] -> ([SectionChunk], [Chunk])
  takeDate d cs = go [] cs
   where
    go :: [SectionChunk] -> [Chunk] -> ([SectionChunk], [Chunk])
    go acc [] = (acc, [])
    go acc cs'@(ChunkDatedChunk (DatedChunk d' content) : csrest)
      | d == d' = go (acc <> [DatedSectionChunk content]) csrest
      | otherwise = (acc, cs')
    go
      acc
      cs'@( ChunkUndatedChunk (UndatedChunk content)
              : ChunkDatedChunk (DatedChunk d' content')
              : csrest
            )
        | d == d' = go (acc <> [UndatedSectionChunk content, DatedSectionChunk content']) csrest
        | otherwise = (acc, cs')
    go acc cs' = (acc, cs')

  groupDatedWithInterspersedUndated :: [Chunk] -> [Section]
  groupDatedWithInterspersedUndated [] = []
  groupDatedWithInterspersedUndated (ChunkUndatedChunk uc : hs) =
    UndatedSection (getUndatedChunk uc) : groupDatedWithInterspersedUndated hs
  groupDatedWithInterspersedUndated
    cs@((ChunkDatedChunk (DatedChunk d _)) : _) = DatedSection d sc : groupDatedWithInterspersedUndated rest
     where
      (sc, rest) = takeDate d cs

-- | Serializes two lists into a diff list of sections
--
-- The following should be true for chronologically sorted inputs:
--
--   * The output list is also sorted by date (if present) and is stable.
--   * If a date appears in both files, its sections result in a 'Both' entry.
--
-- >>> :{
--   serializeTwoSectionsIntoOneDiff
--     [ (DatedSection (fromGregorian 2021 5 1) [DatedSectionChunk "1\n"])
--     , (UndatedSection "\n")
--     , (DatedSection (fromGregorian 2021 5 3) [DatedSectionChunk "3\n"])]
--     [ (DatedSection (fromGregorian 2021 5 1) [DatedSectionChunk "1\n"])
--     , (UndatedSection "\n")
--     , (DatedSection (fromGregorian 2021 5 2) [DatedSectionChunk "2\n"])
--     , (UndatedSection "\n")
--     , (DatedSection (fromGregorian 2021 5 4) [DatedSectionChunk "4\n"])]
-- :}
-- [Both (DatedSection 2021-05-01 [DatedSectionChunk "1\n"]) (DatedSection 2021-05-01 [DatedSectionChunk "1\n"]),First (UndatedSection "\n"),Second (UndatedSection "\n"),Second (DatedSection 2021-05-02 [DatedSectionChunk "2\n"]),Second (UndatedSection "\n"),First (DatedSection 2021-05-03 [DatedSectionChunk "3\n"]),Second (DatedSection 2021-05-04 [DatedSectionChunk "4\n"])]
--
-- >>> :{
--   serializeTwoSectionsIntoOneDiff
--     [ (DatedSection (fromGregorian 2021 5 9) [DatedSectionChunk "9\n"])
--     , (UndatedSection "l0\n")
--     , (DatedSection (fromGregorian 2021 5 14) [DatedSectionChunk "14\n"])]
--     [ (DatedSection (fromGregorian 2021 5 10) [DatedSectionChunk "10\n"])
--     , (UndatedSection "r0\n")
--     , (DatedSection (fromGregorian 2021 5 14) [DatedSectionChunk "14\n"])]
-- :}
-- [First (DatedSection 2021-05-09 [DatedSectionChunk "9\n"]),First (UndatedSection "l0\n"),Second (DatedSection 2021-05-10 [DatedSectionChunk "10\n"]),Second (UndatedSection "r0\n"),Both (DatedSection 2021-05-14 [DatedSectionChunk "14\n"]) (DatedSection 2021-05-14 [DatedSectionChunk "14\n"])]
serializeTwoSectionsIntoOneDiff :: [Section] -> [Section] -> [Diff Section]
serializeTwoSectionsIntoOneDiff [] [] = []
serializeTwoSectionsIntoOneDiff (l : ls) [] = First l : serializeTwoSectionsIntoOneDiff ls []
serializeTwoSectionsIntoOneDiff (l@(DatedSection d0 _) : ls) (r@(DatedSection d1 _) : rs) =
  case compare d0 d1 of
    EQ -> Both l r : serializeTwoSectionsIntoOneDiff ls rs
    LT -> First l : serializeTwoSectionsIntoOneDiff ls (r : rs)
    GT -> Second r : serializeTwoSectionsIntoOneDiff (l : ls) rs
serializeTwoSectionsIntoOneDiff (l@(UndatedSection _) : ls) rs =
  First l : serializeTwoSectionsIntoOneDiff ls rs
serializeTwoSectionsIntoOneDiff [] (r : rs) = Second r : serializeTwoSectionsIntoOneDiff [] rs
serializeTwoSectionsIntoOneDiff ls (r@(UndatedSection _) : rs) = Second r : serializeTwoSectionsIntoOneDiff ls rs

data SmartPieceStatus d
  = Filler
  | Ordered d
  deriving stock (Eq)

class SmartPiece p where
  type SmartPieceOrder p
  type SmartPieceContent p

  getSpContent :: p -> SmartPieceContent p
  getSpStatus :: p -> SmartPieceStatus (SmartPieceOrder p)
  isSpOrdered :: p -> Bool
  isSpOrdered p = case getSpStatus p of
    Filler -> False
    (Ordered _) -> True

instance SmartPiece Section where
  type SmartPieceOrder Section = Day
  type SmartPieceContent Section = Section

  getSpContent = id
  getSpStatus (UndatedSection _) = Filler
  getSpStatus (DatedSection d _) = Ordered d

instance SmartPiece SectionChunk where
  type SmartPieceOrder SectionChunk = Int
  type SmartPieceContent SectionChunk = Text

  getSpContent = sectionChunkToText
  getSpStatus (UndatedSectionChunk _) = Filler
  getSpStatus (DatedSectionChunk _) = Ordered 0

instance SmartPiece (Maybe Int, Text) where
  type SmartPieceOrder (Maybe Int, Text) = Int
  type SmartPieceContent (Maybe Int, Text) = Text

  getSpContent = snd
  getSpStatus (Nothing, _) = Filler
  getSpStatus (Just d, _) = Ordered d

-- | Finds a good matching of filler ranges.
--
-- A good matching is such that for each diff between nonfiller ranges, we
-- match the latest filler ranges before them.
matchFillerRanges :: (SmartPiece p) => [Diff p] -> [Diff (SmartPieceContent p)]
matchFillerRanges [] = []
matchFillerRanges (p0@(First f0) : ps0)
  | isSpOrdered f0 = First (getSpContent f0) : matchFillerRanges ps0
  | otherwise = case ps0 of
    (p1@(Second s1) : ps1) ->
      if isSpOrdered s1
        then Second (getSpContent s1) : matchFillerRanges (p0 : ps1)
        else case ps1 of
          (First f2 : rest3@(First _ : _)) ->
            First (getSpContent f0) : First (getSpContent f2) : matchFillerRanges (p1 : rest3)
          (Second s2 : rest3@(Second _ : _)) ->
            Second (getSpContent s1) : Second (getSpContent s2) : matchFillerRanges (p0 : rest3)
          rest ->
            Both (getSpContent f0) (getSpContent s1) : matchFillerRanges rest
    (p1@(First f1) : ps1) -> case ps1 of
      [] -> First (getSpContent f0) : matchFillerRanges ps0
      (First _ : _) -> First (getSpContent f0) : matchFillerRanges ps0
      (Both _ _ : _) -> First (getSpContent f0) : matchFillerRanges ps0
      (Second s2 : ps2) ->
        if isSpOrdered s2
          then First (getSpContent f0) : matchFillerRanges ps0
          else case ps2 of
            (First _ : _) ->
              First (getSpContent f0) : First (getSpContent f1) : matchFillerRanges ps1
            _ ->
              Both (getSpContent f0) (getSpContent s2) : matchFillerRanges (p1 : ps2)
    ((Both l r) : ps1) ->
      First (getSpContent f0) : Both (getSpContent l) (getSpContent r) : matchFillerRanges ps1
    [] -> [First (getSpContent f0)]
matchFillerRanges ps@((Second _) : _) = flipDiff <$> matchFillerRanges (flipDiff <$> ps)
 where
  flipDiff (First f) = Second f
  flipDiff (Second s) = First s
  flipDiff (Both l r) = Both r l
matchFillerRanges ((Both l r) : xs) =
  Both (getSpContent l) (getSpContent r) : matchFillerRanges xs

-- | Generates ed hunks that are compatible with Vim.
--
-- Until https://github.com/neovim/neovim/issues/14522 is fixed, we need to
-- merge all diffs back together to avoid separate append and delete
-- instructions.
generateAVimCompatibleDiff :: [[Diff Text]] -> [EdHunk]
generateAVimCompatibleDiff = groupedDiffToEdHunks . one . fold

diffPairedSections :: [SectionChunk] -> [SectionChunk] -> [Diff SectionChunk]
diffPairedSections = getDiffBy similarChunk
 where
  similarChunk (UndatedSectionChunk _) _ = False
  similarChunk (DatedSectionChunk _) (UndatedSectionChunk _) = False
  similarChunk (DatedSectionChunk l) (DatedSectionChunk r) =
    hasBoth $ getDiffBy similarLedgerLine (lines l) (lines r)
   where
    isBoth (Both _ _) = True
    isBoth _ = False

    hasBoth = any isBoth

smartlyDiffText :: Diff Text -> [Diff Text]
smartlyDiffText (First l) = First <$> lines l
smartlyDiffText (Second r) = Second <$> lines r
smartlyDiffText (Both ls rs) = getDiffBy similarLedgerLine (lines ls) (lines rs)

matchInternalChunks :: [Diff Section] -> [Diff SectionChunk]
matchInternalChunks [] = []
matchInternalChunks (First l : xs) =
  (First <$> sectionToSectionChunks l)
    <> matchInternalChunks xs
matchInternalChunks (Second l : xs) =
  (Second <$> sectionToSectionChunks l)
    <> matchInternalChunks xs
matchInternalChunks (Both l r : xs) =
  diffPairedSections lc rc
    <> matchInternalChunks xs
 where
  (lc, rc) = both sectionToSectionChunks (l, r)

-- | Runs a chronological diff on two Ledger files.
--
-- Outputs an ed-style diff.
diffLedger :: Journal -> Journal -> Text
diffLedger (Journal origChunks) (Journal destChunks) =
  edHunksToEdDiff $
    generateAVimCompatibleDiff $
      smartlyDiffText <$> matchedSectionChunksWithFillerLines
 where
  sections :: ([Section], [Section]) =
    both groupChunksIntoSections (origChunks, destChunks)
  sectionsMatchedByDate :: [Diff Section] =
    uncurry serializeTwoSectionsIntoOneDiff sections
  matchedSectionChunks :: [Diff SectionChunk] = matchInternalChunks sectionsMatchedByDate
  matchedSectionChunksWithFillerLines :: [Diff Text] =
    matchFillerRanges matchedSectionChunks

-- | Runs a chronological diff on two Ledger files.
--
-- Outputs an ed-style diff.
diffLedgerText :: Text -> Text -> Either Text Text
diffLedgerText orig dest = do
  origJ <- parseJournal orig
  destJ <- parseJournal dest
  return $ diffLedger origJ destJ

-- | Runs a chronological diff on two Ledger files.
--
-- Outputs an ed-style diff.
diffLedgerIO :: FilePath -> FilePath -> IO (Either Text Text)
diffLedgerIO original new = do
  originalContent <- readFileText original
  newContent <- readFileText new
  return $ diffLedgerText originalContent newContent
