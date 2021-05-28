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

chunkToTaggedPair :: Chunk -> (Maybe Day, Text)
chunkToTaggedPair (ChunkUndatedChunk (UndatedChunk content)) = (Nothing, content)
chunkToTaggedPair (ChunkDatedChunk (DatedChunk day content)) = (Just day, content)

-- | Groups together consecutive undated chunks as well as same-dated chunks
-- interspersed with undated ones.
--
-- >>> :{
--   groupSameValueSections
--     [ (Just 1, "1"), (Nothing, "yo"), (Nothing, "lo"), (Just 1, "2") ]
-- :}
-- [(Just 1,"1yolo2")]
groupSameValueSections :: (Monoid a, Eq d) => [(Maybe d, a)] -> [(Maybe d, a)]
groupSameValueSections = groupJustWithNothings . groupNothings
 where
  groupNothings [] = []
  groupNothings ((Nothing, x) : ((Nothing, y) : ys)) = groupNothings ((Nothing, x <> y) : ys)
  groupNothings (x : xs) = x : groupNothings xs

  groupJustWithNothings :: (Semigroup a, Eq d) => [(Maybe d, a)] -> [(Maybe d, a)]
  groupJustWithNothings [] = []
  groupJustWithNothings (h@(Nothing, _) : hs) = h : groupJustWithNothings hs
  groupJustWithNothings ((Just d0, c0) : (Just d1, c1) : hs)
    | d0 == d1 = groupJustWithNothings $ (Just d0, c0 <> c1) : hs
    | otherwise = (Just d0, c0) : groupJustWithNothings ((Just d1, c1) : hs)
  groupJustWithNothings ((Just d0, c0) : (Nothing, cm) : (Just d1, c1) : hs)
    | d0 == d1 = groupJustWithNothings $ (Just d0, c0 <> cm <> c1) : hs
    | otherwise = (Just d0, c0) : groupJustWithNothings ((Nothing, cm) : (Just d1, c1) : hs)
  groupJustWithNothings (h : hs) = h : groupJustWithNothings hs

-- | This data structures serializes a hunk in a dated diff
data IntermediateDiff d
  = BothSame d (Text, Text)
  | LeftChunk (Maybe d, Text)
  | RightChunk (Maybe d, Text)
  deriving stock (Eq, Show)

flipIntermediateDiff :: IntermediateDiff d -> IntermediateDiff d
flipIntermediateDiff (BothSame d p) = BothSame d (swap p)
flipIntermediateDiff (LeftChunk a) = RightChunk a
flipIntermediateDiff (RightChunk a) = LeftChunk a

-- | Serializes two lists of dated chunks.
--
-- The following should be true for chronologically sorted inputs:
--
--   * The output list is also sorted by date (if present) and is stable.
--   * If a date appears in both files, its chunks result in 'BothSame'.
--   * Undated chunks appear before dated chunks.
--
-- >>> :{
--   mergeDatedChunks
--     [ (Just 1, "1"), (Nothing, ""), (Just 3, "3")]
--     [(Just 1, "1"), (Nothing, ""), (Just 2, "2"), (Nothing, ""), (Just 4, "4")]
-- :}
-- [BothSame 1 ("1","1"),LeftChunk (Nothing,""),RightChunk (Nothing,""),RightChunk (Just 2,"2"),RightChunk (Nothing,""),LeftChunk (Just 3,"3"),RightChunk (Just 4,"4")]
--
-- >>> :{
--   mergeDatedChunks
--     [(Just 9, "9"), (Nothing, "l0"), (Just 14, "14")]
--     [(Just 10, "10"), (Nothing, "r0"), (Just 14, "14")]
-- :}
-- [LeftChunk (Just 9,"9"),LeftChunk (Nothing,"l0"),RightChunk (Just 10,"10"),RightChunk (Nothing,"r0"),BothSame 14 ("14","14")]
mergeDatedChunks :: (Ord d) => [(Maybe d, Text)] -> [(Maybe d, Text)] -> [IntermediateDiff d]
mergeDatedChunks [] [] = []
mergeDatedChunks (l@(Just d0, x) : ls) (r@(Just d1, y) : rs) =
  case compare d0 d1 of
    EQ -> BothSame d0 (x, y) : mergeDatedChunks ls rs
    LT -> LeftChunk l : mergeDatedChunks ls (r : rs)
    GT -> RightChunk r : mergeDatedChunks (l : ls) rs
mergeDatedChunks ((Nothing, x0) : ls) rs =
  LeftChunk (Nothing, x0) : mergeDatedChunks ls rs
mergeDatedChunks ls rs@((Nothing, _) : _) = flipIntermediateDiff <$> mergeDatedChunks rs ls
mergeDatedChunks [] (r@(Just _, _) : rs) = RightChunk r : mergeDatedChunks [] rs
mergeDatedChunks (l@(Just _, _) : ls) [] = LeftChunk l : mergeDatedChunks [] ls

-- | Finds an optimal matching of undated chunks.
--
-- The optimal matching is such that for each diff between dated chunks, we
-- match the latest undated chunk before them.
--
-- >>> :{
--   matchUndatedChunks
--     [LeftChunk (Nothing,"l0"),RightChunk (Nothing,"r0"),
--      RightChunk (Just 2,"2"),RightChunk (Nothing,"r1"),
--      LeftChunk (Just 3,"3"),RightChunk (Just 4,"4")]
-- :}
-- [("","r0"),("","2"),("l0","r1"),("3",""),("","4")]
--
-- >>> :{
--   matchUndatedChunks
--     [LeftChunk (Just 9, "9"), LeftChunk (Nothing,"l0"),
--      RightChunk (Just 10,"10"),RightChunk (Nothing,"r0"),
--      BothSame 14 ("14","14")]
-- :}
-- [("9",""),("","10"),("l0","r0"),("14","14")]
matchUndatedChunks :: (Ord d) => [IntermediateDiff d] -> [(Text, Text)]
matchUndatedChunks [] = []
matchUndatedChunks (h0@(LeftChunk (Nothing, lc)) : rest) =
  case rest of
    (h@(RightChunk (Nothing, rc)) : rest') ->
      case rest' of
        (LeftChunk (Just _, lc') : h'@(LeftChunk (Nothing, _) : _)) ->
          (lc, "") : (lc', "") : matchUndatedChunks (h : h')
        (RightChunk (Just _, rc') : h'@(RightChunk (Nothing, _) : _)) ->
          ("", rc) : ("", rc') : matchUndatedChunks (h0 : h')
        _ -> (lc, rc) : matchUndatedChunks rest'
    ((RightChunk (Just _, rc)) : rest') -> ("", rc) : matchUndatedChunks (h0 : rest')
    _ -> (lc, "") : matchUndatedChunks rest
matchUndatedChunks m@((RightChunk (Nothing, _)) : _) =
  swap <$> matchUndatedChunks (flipIntermediateDiff <$> m)
matchUndatedChunks ((RightChunk (Just _, r)) : xs) = ("", r) : matchUndatedChunks xs
matchUndatedChunks ((LeftChunk (Just _, l)) : xs) = (l, "") : matchUndatedChunks xs
matchUndatedChunks ((BothSame _ (l, r)) : xs) = (l, r) : matchUndatedChunks xs

-- | Runs a chronological diff on two Ledger files.
--
-- Outputs an ed-style diff.
diffLedger :: Journal -> Journal -> Text
diffLedger (Journal origChunks) (Journal destChunks) = edHunksToEdDiff edHunks
 where
  (origSameDayGroupedChunks, destSameDayGroupedChunks :: [(Maybe Day, Text)]) =
    both
      (groupSameValueSections . fmap chunkToTaggedPair)
      (origChunks, destChunks)
  diffsGroupedByDate :: [[Diff Text]] =
    fmap (uncurry (getDiffBy similarLedgerLine) . both lines) $
      matchUndatedChunks $
        mergeDatedChunks origSameDayGroupedChunks destSameDayGroupedChunks
  -- Until https://github.com/neovim/neovim/issues/14522 is fixed, we need to merge all diffs back together to avoid
  -- separate append and delete instructions)
  allDiffsForNeovim :: [Diff Text] = fold diffsGroupedByDate
  edHunks :: [EdHunk] = groupedDiffToEdHunks [allDiffsForNeovim]

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
