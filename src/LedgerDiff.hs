-- For doctests
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- | Generates a smart diff of ledger files.
module LedgerDiff (
  diffLedgerText,
  diffLedgerIO,
) where

import Data.Algorithm.Diff (
  Diff,
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
  deriving stock (Show)

sectionChunkToText :: SectionChunk -> Text
sectionChunkToText (DatedSectionChunk content) = content
sectionChunkToText (UndatedSectionChunk content) = content

-- | A section is a grouping of chunks.
data Section
  = UndatedSection Text
  | DatedSection Day [SectionChunk]
  deriving stock (Show)

sectionToTaggedPair :: Section -> (Maybe Day, Text)
sectionToTaggedPair (UndatedSection c) = (Nothing, c)
sectionToTaggedPair (DatedSection d c) = (Just d, fold $ sectionChunkToText <$> c)

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
-- [DatedSection 2021-05-30 [DatedSectionChunk "1yolo2"]]
groupChunksIntoSections :: [Chunk] -> [Section]
groupChunksIntoSections = groupDatedWithInterespersedUndated . groupUndated
 where
  groupUndated :: [Chunk] -> [Chunk]
  groupUndated [] = []
  groupUndated (ChunkUndatedChunk uc : (ChunkUndatedChunk uc' : cs)) =
    groupUndated (ChunkUndatedChunk (uc <> uc') : cs)
  groupUndated (x : xs) = x : groupUndated xs

  groupDatedWithInterespersedUndated :: [Chunk] -> [Section]
  groupDatedWithInterespersedUndated [] = []
  groupDatedWithInterespersedUndated (ChunkUndatedChunk uc : hs) =
    UndatedSection (getUndatedChunk uc) : groupDatedWithInterespersedUndated hs
  groupDatedWithInterespersedUndated
    ( (ChunkDatedChunk (DatedChunk d0 c0))
        : rest@(ChunkDatedChunk (DatedChunk d1 c1) : hs)
      )
      | d0 == d1 = groupDatedWithInterespersedUndated $ ChunkDatedChunk (DatedChunk d0 $ c0 <> c1) : hs
      | otherwise = DatedSection d0 [DatedSectionChunk c0] : groupDatedWithInterespersedUndated rest
  groupDatedWithInterespersedUndated
    ( ChunkDatedChunk (DatedChunk d0 c0)
        : rest@(ChunkUndatedChunk (UndatedChunk cu) : ChunkDatedChunk (DatedChunk d1 c1) : hs)
      )
      | d0 == d1 = groupDatedWithInterespersedUndated $ ChunkDatedChunk (DatedChunk d0 (c0 <> cu <> c1)) : hs
      | otherwise = DatedSection d0 [DatedSectionChunk c0] : groupDatedWithInterespersedUndated rest
  groupDatedWithInterespersedUndated (ChunkDatedChunk (DatedChunk d0 c0) : rest) =
    DatedSection d0 [DatedSectionChunk c0] : groupDatedWithInterespersedUndated rest

-- | 'IntermediateDiff' represents parts of two diffed objects and whether they
-- stand alone or represent the same section in both.
data IntermediateDiff d c
  = Matched (c, c)
  | LeftChunk (Maybe d, c)
  | RightChunk (Maybe d, c)
  deriving stock (Eq, Show)

flipIntermediateDiff :: IntermediateDiff d c -> IntermediateDiff d c
flipIntermediateDiff (Matched p) = Matched (swap p)
flipIntermediateDiff (LeftChunk a) = RightChunk a
flipIntermediateDiff (RightChunk a) = LeftChunk a

-- | Serializes two lists of dated chunks.
--
-- The following should be true for chronologically sorted inputs:
--
--   * The output list is also sorted by date (if present) and is stable.
--   * If a date appears in both files, its chunks result in 'Matched'.
--   * Undated chunks appear before dated chunks.
--
-- >>> :{
--   mergeDatedSections
--     [ (Just 1, "1"), (Nothing, ""), (Just 3, "3")]
--     [(Just 1, "1"), (Nothing, ""), (Just 2, "2"), (Nothing, ""), (Just 4, "4")]
-- :}
-- [Matched ("1","1"),LeftChunk (Nothing,""),RightChunk (Nothing,""),RightChunk (Just 2,"2"),RightChunk (Nothing,""),LeftChunk (Just 3,"3"),RightChunk (Just 4,"4")]
--
-- >>> :{
--   mergeDatedSections
--     [(Just 9, "9"), (Nothing, "l0"), (Just 14, "14")]
--     [(Just 10, "10"), (Nothing, "r0"), (Just 14, "14")]
-- :}
-- [LeftChunk (Just 9,"9"),LeftChunk (Nothing,"l0"),RightChunk (Just 10,"10"),RightChunk (Nothing,"r0"),Matched ("14","14")]
mergeDatedSections ::
  (Ord d, Semigroup section) =>
  [(Maybe d, section)] ->
  [(Maybe d, section)] ->
  [IntermediateDiff d section]
mergeDatedSections [] [] = []
mergeDatedSections (l@(Just d0, x) : ls) (r@(Just d1, y) : rs) =
  case compare d0 d1 of
    EQ -> Matched (x, y) : mergeDatedSections ls rs
    LT -> LeftChunk l : mergeDatedSections ls (r : rs)
    GT -> RightChunk r : mergeDatedSections (l : ls) rs
mergeDatedSections ((Nothing, x0) : ls) rs =
  LeftChunk (Nothing, x0) : mergeDatedSections ls rs
mergeDatedSections ls rs@((Nothing, _) : _) = flipIntermediateDiff <$> mergeDatedSections rs ls
mergeDatedSections [] (r@(Just _, _) : rs) = RightChunk r : mergeDatedSections [] rs
mergeDatedSections (l@(Just _, _) : ls) [] = LeftChunk l : mergeDatedSections [] ls

-- | Finds a good matching of undated sections.
--
-- A good matching is such that for each diff between dated chunks, we
-- match the latest undated chunk before them.
--
-- >>> :{
--   matchUndatedSections
--     [LeftChunk (Nothing,"l0"),RightChunk (Nothing,"r0"),
--      RightChunk (Just 2,"2"),RightChunk (Nothing,"r1"),
--      LeftChunk (Just 3,"3"),RightChunk (Just 4,"4")]
-- :}
-- [("","r0"),("","2"),("l0","r1"),("3",""),("","4")]
--
-- >>> :{
--   matchUndatedSections
--     [LeftChunk (Just 9, "9"), LeftChunk (Nothing,"l0"),
--      RightChunk (Just 10,"10"),RightChunk (Nothing,"r0"),
--      Matched ("14","14")]
-- :}
-- [("9",""),("","10"),("l0","r0"),("14","14")]
matchUndatedSections :: (Ord d, Monoid c) => [IntermediateDiff d c] -> [(c, c)]
matchUndatedSections [] = []
matchUndatedSections (h0@(LeftChunk (Nothing, lc)) : rest) =
  case rest of
    (h@(RightChunk (Nothing, rc)) : rest') ->
      case rest' of
        (LeftChunk (Just _, lc') : h'@(LeftChunk (Nothing, _) : _)) ->
          (lc, mempty) : (lc', mempty) : matchUndatedSections (h : h')
        (RightChunk (Just _, rc') : h'@(RightChunk (Nothing, _) : _)) ->
          (mempty, rc) : (mempty, rc') : matchUndatedSections (h0 : h')
        _ -> (lc, rc) : matchUndatedSections rest'
    ((RightChunk (Just _, rc)) : rest') -> (mempty, rc) : matchUndatedSections (h0 : rest')
    _ -> (lc, mempty) : matchUndatedSections rest
matchUndatedSections m@((RightChunk (Nothing, _)) : _) =
  swap <$> matchUndatedSections (flipIntermediateDiff <$> m)
matchUndatedSections ((RightChunk (Just _, r)) : xs) = (mempty, r) : matchUndatedSections xs
matchUndatedSections ((LeftChunk (Just _, l)) : xs) = (l, mempty) : matchUndatedSections xs
matchUndatedSections ((Matched (l, r)) : xs) = (l, r) : matchUndatedSections xs

-- | Generates ed hunks that are compatible with Vim.
--
-- Until https://github.com/neovim/neovim/issues/14522 is fixed, we need to
-- merge all diffs back together to avoid separate append and delete
-- instructions.
generateAVimCompatibleDiff :: [[Diff Text]] -> [EdHunk]
generateAVimCompatibleDiff = groupedDiffToEdHunks . one . fold

-- | Runs a chronological diff on two Ledger files.
--
-- Outputs an ed-style diff.
diffLedger :: Journal -> Journal -> Text
diffLedger (Journal origChunks) (Journal destChunks) =
  edHunksToEdDiff $
    generateAVimCompatibleDiff diffsGroupedByDate
 where
  sections :: ([Section], [Section]) =
    both groupChunksIntoSections (origChunks, destChunks)
  diffsGroupedByDate :: [[Diff Text]] =
    fmap (uncurry (getDiffBy similarLedgerLine) . both lines) $
      matchUndatedSections . uncurry mergeDatedSections $ both (fmap sectionToTaggedPair) sections

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
