{-# LANGUAGE OverloadedLists #-}

-- | This module handles transforming diffs into ed-style diff files.
module Ed (
  EdHunk,
  edHunksToEdDiff,
  groupedDiffToEdHunks,
) where

import Data.Algorithm.Diff (
  Diff,
  PolyDiff (Both, First, Second),
 )
import Data.List.NonEmpty (groupBy)
import Data.Sequence (Seq (..))
import Data.Text qualified as T
import Relude hiding (First)

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
  T.intercalate "\n"
    . toList
    $ ("> " <>)
    <$> as
printHunkBody (EdHunkTypeDelete (EdHunkDelete ds)) =
  T.intercalate "\n"
    . toList
    $ ("< " <>)
    <$> ds
printHunkBody (EdHunkTypeChange (EdHunkChange ds as)) =
  printHunkBody (EdHunkTypeDelete (EdHunkDelete ds))
    <> "\n"
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
    -- \| the offset and changes
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
    return
      . EdHunkTypeChange
      $ EdHunkChange
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
