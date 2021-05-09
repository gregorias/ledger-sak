module LedgerDiff (
  diff,
  diffLedgerText,
  diffLedgerIO,
) where

import Data.Algorithm.Diff (Diff, PolyDiff (Both, First, Second), getDiff, getDiffBy)
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

newtype EdHunkAppend = EdHunkAppend
  { _ehaDestLines :: NonEmpty Text
  }

newtype EdHunkDelete = EdHunkDelete
  { _ehdOrigLines :: NonEmpty Text
  }

data EdHunkChange = EdHunkChange
  { _ehcOrigLines :: NonEmpty Text
  , _ehcDestLines :: NonEmpty Text
  }

data EdHunkType
  = EdHunkTypeAppend !EdHunkAppend
  | EdHunkTypeDelete !EdHunkDelete
  | EdHunkTypeChange !EdHunkChange

data EdHunk = EdHunk
  { ehOrigPos :: !Int
  , ehDestPos :: !Int
  , ehType :: !EdHunkType
  }

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

diffToEdHunks :: [Diff Text] -> [EdHunk]
diffToEdHunks = snd . diffToEdHunks' mempty

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

groupChunksByDay :: [Chunk] -> [(Maybe Day, Text)]
groupChunksByDay = go (Nothing, []) . fmap chunkToDT
 where
  chunkToDT (ChunkUndatedChunk (UndatedChunk content)) = (Nothing, content)
  chunkToDT (ChunkDatedChunk (DatedChunk day content)) = (Just day, content)
  go :: (Maybe Day, Seq Text) -> [(Maybe Day, Text)] -> [(Maybe Day, Text)]
  go (currentDay, acc) [] = [(currentDay, fold acc)]
  go (currentDay, acc) ((nextDay, content) : xs)
    | currentDay == nextDay || isNothing nextDay = go (currentDay, acc :|> content) xs
    | otherwise = (currentDay, fold acc) : go (nextDay, [content]) xs

mergeByKey ::
  (Monoid a, Monoid b, Ord key) =>
  [(key, a)] ->
  [(key, b)] ->
  [(a, b)]
mergeByKey as bs = go as' bs'
 where
  group' :: (Eq key, Monoid a) => [(key, a)] -> [(key, a)]
  group' = fmap merge . groupBy ((==) `on` fst)
   where
    merge :: (Monoid a) => NonEmpty (key, a) -> (key, a)
    merge ((key, e) :| es) = (key, fold (e : (snd <$> es)))
  (as', bs') = (group' as, group' bs)
  go :: (Monoid a, Monoid b, Ord key) => [(key, a)] -> [(key, b)] -> [(a, b)]
  go as'' [] = fmap (\(_, a) -> (a, mempty)) as''
  go [] bs'' = fmap (\(_, a) -> (mempty, a)) bs''
  go ls@((ka, a) : as'') rs@((kb, b) : bs'')
    | ka == kb = (a, b) : go as'' bs''
    | ka < kb = (a, mempty) : go as'' rs
    | otherwise = (mempty, b) : go ls bs''

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

-- | Runs a chronological diff on two Ledger files.
--
-- Outputs an ed-style diff.
diffLedger :: Journal -> Journal -> Text
diffLedger (Journal origChunks) (Journal destChunks) = edHunksToEdDiff edHunks
 where
  origChunksGroupedByDate :: [(Maybe Day, [Text])] = lines <<$>> groupChunksByDay origChunks
  destChunksGroupedByDate :: [(Maybe Day, [Text])] = lines <<$>> groupChunksByDay destChunks
  chunksGroupedByDate :: [([Text], [Text])] = mergeByKey origChunksGroupedByDate destChunksGroupedByDate
  diffsGroupedByDate :: [[Diff Text]] = uncurry (getDiffBy similarLedgerLine) <$> chunksGroupedByDate
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
diff :: Text -> Text -> Text
diff original new = edHunksToEdDiff . diffToEdHunks $ diffResult
 where
  (originalLines, newLines) = (lines original, lines new)
  diffResult :: [Diff Text] = getDiff originalLines newLines

-- | Runs a chronological diff on two Ledger files.
--
-- Outputs an ed-style diff.
diffLedgerIO :: FilePath -> FilePath -> IO (Either Text Text)
diffLedgerIO original new = do
  originalContent <- readFileText original
  newContent <- readFileText new
  return $ diffLedgerText originalContent newContent
