module LedgerDiff (
  diffLedgerText,
  diffLedgerIO,
) where

import Data.Algorithm.Diff (
  Diff,
  PolyDiff (Both, First, Second),
  getDiffBy,
 )
import Data.List.NonEmpty (groupBy)
import Data.Sequence (Seq (..))
import qualified Data.Text as T
import Data.Time (Day)
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

newtype EdHunkAppend = EdHunkAppend
  { _ehaDestLines :: NonEmpty Text
  }
  deriving newtype (Eq, Show)

newtype EdHunkDelete = EdHunkDelete
  { _ehdOrigLines :: NonEmpty Text
  }
  deriving newtype (Eq, Show)

data EdHunkChange = EdHunkChange
  { _ehcOrigLines :: NonEmpty Text
  , _ehcDestLines :: NonEmpty Text
  }
  deriving stock (Eq, Show)

data EdHunkType
  = EdHunkTypeAppend !EdHunkAppend
  | EdHunkTypeDelete !EdHunkDelete
  | EdHunkTypeChange !EdHunkChange
  deriving stock (Eq, Show)

data EdHunk = EdHunk
  { ehOrigPos :: !Int
  , ehDestPos :: !Int
  , ehType :: !EdHunkType
  }
  deriving stock (Eq, Show)

-- | Prints a segment line spec
--
-- >>> printSegmentSpec 0 0
-- "0"
--
-- >>> printSegmentSpec 0 1
-- "1"
--
-- >>> printSegmentSpec 0 2
-- "1,2"
printSegmentSpec :: Int -> Int -> Text
printSegmentSpec origPos 0 = show origPos
printSegmentSpec origPos lineCount@(1) = show $ origPos + lineCount
printSegmentSpec origPos lineCount =
  show (origPos + 1) <> "," <> show (origPos + lineCount)

-- | Prints the single hunk's head.
--
-- >>> printHunkHead (EdHunk 0 0 (EdHunkTypeAppend (EdHunkAppend ["new"])))
-- "0a1"
--
-- >>> printHunkHead (EdHunk 0 0 (EdHunkTypeDelete (EdHunkDelete ["old"])))
-- "1d0"
--
-- >>> printHunkHead (EdHunk 0 0 (EdHunkTypeChange (EdHunkChange ["old0", "old1"] ["new0"])))
-- "1,2c1"
printHunkHead :: EdHunk -> Text
printHunkHead
  EdHunk
    { ehOrigPos = origPos
    , ehDestPos = destPos
    , ehType = EdHunkTypeAppend (EdHunkAppend ds)
    } =
    printSegmentSpec origPos 0 <> "a" <> printSegmentSpec destPos (length ds)
printHunkHead
  EdHunk
    { ehOrigPos = origPos
    , ehDestPos = destPos
    , ehType = EdHunkTypeDelete (EdHunkDelete ds)
    } =
    printSegmentSpec origPos (length ds) <> "d" <> printSegmentSpec destPos 0
printHunkHead
  EdHunk
    { ehOrigPos = origPos
    , ehDestPos = destPos
    , ehType = EdHunkTypeChange (EdHunkChange os ds)
    } =
    printSegmentSpec origPos (length os) <> "c" <> printSegmentSpec destPos (length ds)

-- | Prints the hunk's body, i.e., the changed lines
--
-- >>> printHunkBody (EdHunkTypeAppend (EdHunkAppend ["000", "111"]))
-- "> 000\n> 111"
--
-- >>> printHunkBody (EdHunkTypeDelete (EdHunkDelete ["000", "111"]))
-- "< 000\n< 111"
printHunkBody :: EdHunkType -> Text
printHunkBody (EdHunkTypeAppend (EdHunkAppend as)) =
  T.intercalate "\n" . toList $
    ("> " <>) <$> as
printHunkBody (EdHunkTypeDelete (EdHunkDelete ds)) =
  T.intercalate "\n" . toList $
    ("< " <>) <$> ds
printHunkBody (EdHunkTypeChange (EdHunkChange ds as)) =
  printHunkBody (EdHunkTypeDelete (EdHunkDelete ds)) <> "\n"
    <> "---\n"
    <> printHunkBody (EdHunkTypeAppend (EdHunkAppend as))

data FilePos = FilePos
  { fpOrigPos :: !Int
  , fpDestPos :: !Int
  }
  deriving stock (Eq, Show)

instance Semigroup FilePos where
  (<>) (FilePos a b) (FilePos a' b') = FilePos (a + a') (b + b')

instance Monoid FilePos where
  mempty = FilePos 0 0

-- | Transforms a group of diffs to ed-style hunks.
--
-- This is a generic transformer than can start at any position and returns the
-- new position after processing.
diffToEdHunks' ::
  -- | starting position
  FilePos ->
  [Diff Text] ->
  -- | ending position and the resulting hunks
  (FilePos, [EdHunk])
diffToEdHunks' fp =
  foldl' go (fp, [])
    . fmap groupedDiffsToWorkItem
    . group'
 where
  isBoth :: Diff a -> Bool
  isBoth (Both _ _) = True
  isBoth _ = False

  firstsAndSecondsTogether :: (Eq a) => Diff a -> Diff a -> Bool
  firstsAndSecondsTogether a b
    | isBoth a || isBoth b = a == b
    | otherwise = True

  group' :: (Eq a) => [Diff a] -> [NonEmpty (Diff a)]
  group' = groupBy firstsAndSecondsTogether

  groupedDiffsToWorkItem ::
    (Foldable t) =>
    t (Diff Text) ->
    -- | the offset and changes
    (FilePos, Seq Text, Seq Text)
  groupedDiffsToWorkItem = foldl' go' (mempty, [], [])
   where
    go' (fp', fs, ss) (First a) = (fp' <> FilePos 1 0, fs :|> a, ss)
    go' (fp', fs, ss) (Second a) = (fp' <> FilePos 0 1, fs, ss :|> a)
    go' (fp', fs, ss) (Both _ _) = (fp' <> FilePos 1 1, fs, ss)

  wiToEdHunkType :: (Seq Text, Seq Text) -> Maybe EdHunkType
  wiToEdHunkType ([], []) = Nothing
  wiToEdHunkType ([], seconds) = return . EdHunkTypeAppend $ EdHunkAppend (fromList . toList $ seconds)
  wiToEdHunkType (firsts, []) = return . EdHunkTypeDelete $ EdHunkDelete (fromList . toList $ firsts)
  wiToEdHunkType (firsts, seconds) =
    return . EdHunkTypeChange $
      EdHunkChange
        (fromList . toList $ firsts)
        (fromList . toList $ seconds)

  go :: (FilePos, [EdHunk]) -> (FilePos, Seq Text, Seq Text) -> (FilePos, [EdHunk])
  go (fp', ehs) (offset, fs, ss) = (fp' <> offset, ehs <> ehs')
   where
    ehs' = case wiToEdHunkType (fs, ss) of
      Nothing -> []
      Just eht -> [EdHunk (fpOrigPos fp') (fpDestPos fp') eht]

-- | Transforms a collection of diffs to ed-style hunks.
--
-- This function takes maintains the grouping of diffs, i.e., diffs in separate
-- lists will end up in separate hunks.
groupedDiffToEdHunks :: [[Diff Text]] -> [EdHunk]
groupedDiffToEdHunks = snd . foldl' go (mempty, [])
 where
  go :: (FilePos, [EdHunk]) -> [Diff Text] -> (FilePos, [EdHunk])
  go (fp, ehs) ds =
    let (fp', ehs') = diffToEdHunks' fp ds
     in (fp', ehs <> ehs')

edHunkToEdDiff :: EdHunk -> Text
edHunkToEdDiff eh =
  printHunkHead eh <> "\n" <> printHunkBody (ehType eh)

edHunksToEdDiff :: [EdHunk] -> Text
edHunksToEdDiff ehs = T.intercalate "\n" $ edHunkToEdDiff <$> ehs

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
