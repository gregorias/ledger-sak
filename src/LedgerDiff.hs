module LedgerDiff (
  diff,
  diffIO,
) where

import Data.Algorithm.Diff (Diff, PolyDiff (Both, First, Second), getDiff)
import Data.Sequence (Seq (..))
import qualified Data.Text as T
import Relude hiding (First)

-- | Splits the text into lines and keeps the newlines.
--
-- >>> lines' "a\nb\nc"
-- ["a","b","c"]
--
-- >>> lines' "a\nb\n"
-- ["a","b",""]
lines' :: Text -> [Text]
lines' = T.split (== '\n')

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

diffToEdHunks :: [Diff Text] -> [EdHunk]
diffToEdHunks = diffToEdHunks' (0, 0) ([], [])
 where
  wiToEdHunkType :: (Seq Text, Seq Text) -> Maybe EdHunkType
  wiToEdHunkType ([], []) = Nothing
  wiToEdHunkType ([], seconds) = return . EdHunkTypeAppend $ EdHunkAppend (fromList . toList $ seconds)
  wiToEdHunkType (firsts, []) = return . EdHunkTypeDelete $ EdHunkDelete (fromList . toList $ firsts)
  wiToEdHunkType (firsts, seconds) =
    return . EdHunkTypeChange $
      EdHunkChange
        (fromList . toList $ firsts)
        (fromList . toList $ seconds)

  diffToEdHunks' :: (Int, Int) -> (Seq Text, Seq Text) -> [Diff Text] -> [EdHunk]
  diffToEdHunks' pos (firsts, seconds) (First f : diffs) =
    diffToEdHunks' pos (firsts :|> f, seconds) diffs
  diffToEdHunks' pos (firsts, seconds) (Second s : diffs) =
    diffToEdHunks' pos (firsts, seconds :|> s) diffs
  diffToEdHunks' (origPos, destPos) wi@(firsts, seconds) (Both _ _ : diffs) =
    maybe
      []
      (\eht -> [EdHunk{ehOrigPos = origPos, ehDestPos = destPos, ehType = eht}])
      (wiToEdHunkType wi)
      <> diffToEdHunks'
        (origPos + 1 + length firsts, destPos + 1 + length seconds)
        ([], [])
        diffs
  diffToEdHunks' (origPos, destPos) wi [] =
    maybe
      []
      (\eht -> [EdHunk{ehOrigPos = origPos, ehDestPos = destPos, ehType = eht}])
      (wiToEdHunkType wi)

edHunkToEdDiff :: EdHunk -> Text
edHunkToEdDiff eh =
  printHunkHead eh <> "\n" <> printHunkBody (ehType eh)

edHunksToEdDiff :: [EdHunk] -> Text
edHunksToEdDiff ehs = T.intercalate "\n" $ edHunkToEdDiff <$> ehs

-- | Runs a chronological diff on two Ledger files.
--
-- Outputs an ed-style diff.
diff :: Text -> Text -> Text
diff original new = edHunksToEdDiff . diffToEdHunks $ diffResult
 where
  (originalLines, newLines) = (lines' original, lines' new)
  diffResult :: [Diff Text] = getDiff originalLines newLines

-- | Runs a chronological diff on two Ledger files.
--
-- Outputs an ed-style diff.
diffIO :: FilePath -> FilePath -> IO Text
diffIO original new = do
  originalContent <- readFileText original
  newContent <- readFileText new
  return $ diff originalContent newContent
