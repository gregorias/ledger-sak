module LedgerDiff (
  diff,
) where

import Data.Algorithm.Diff (Diff, PolyDiff (Both, First, Second), getDiff)
import qualified Data.Text as T
import Relude hiding (First)

-- | Splits the text into lines and keeps the newlines.
--
-- >>> lines' "a\nb\nc"
-- ["a\n","b\n","c"]
--
-- >>> lines' "a\nb\n"
-- ["a\n","b\n",""]
lines' :: Text -> [Text]
lines' = addNewlines . T.split (== '\n')
 where
  addNewlines [] = []
  addNewlines [x] = [x]
  addNewlines (x : xs) = (x <> "\n") : addNewlines xs

-- | Prints the single hunk's head.
--
-- >>> printHunkHead 420 69
-- "@@ -1,420 +1,69 @@\n"
printHunkHead :: Int -> Int -> Text
printHunkHead originalSize newSize =
  "@@ -1," <> show originalSize <> " +1," <> show newSize <> " @@\n"

diffToUnifiedDiffHunk :: [Diff Text] -> Text
diffToUnifiedDiffHunk [] = ""
diffToUnifiedDiffHunk (l : ls) =
  ( case l of
      First _ -> "-"
      Second _ -> "+"
      Both _ _ -> " "
  )
    <> getValue l
    <> diffToUnifiedDiffHunk ls
 where
  getValue :: Diff Text -> Text
  getValue (First x) = x
  getValue (Second x) = x
  getValue (Both x _) = x

-- | Runs a chronological diff on two Ledger files.
--
-- Outputs a unified diff hunk.
diffPure :: Text -> Text -> Text
diffPure original new =
  printHunkHead (length originalLines) (length newLines)
    <> diffToUnifiedDiffHunk diffResult
 where
  (originalLines, newLines) = (lines' original, lines' new)
  diffResult :: [Diff Text] = getDiff originalLines newLines

-- | Runs a chronological diff on two Ledger files.
--
-- Outputs a unified diff.
diff :: FilePath -> FilePath -> IO Text
diff original new = do
  originalContent <- readFileText original
  newContent <- readFileText new
  return $ header <> diffPure originalContent newContent
 where
  header =
    ("--- " <> toText original <> "\n")
      <> ("+++ " <> toText new <> "\n")
