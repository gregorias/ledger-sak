module Parse (
  DatedChunk (..),
  UndatedChunk (..),
  Chunk (..),
  Journal (..),
  parseJournal,
) where

import Data.Time (Day, defaultTimeLocale, parseTimeM)
import Relude
import Text.Megaparsec (
  MonadParsec (eof, label),
  Parsec,
  anySingle,
  errorBundlePretty,
  lookAhead,
  manyTill,
  manyTill_,
  match,
  parse,
  single,
  someTill,
  takeWhileP,
  try,
 )
import Text.Megaparsec.Char (
  char,
  letterChar,
  newline,
  printChar,
  spaceChar,
 )

data DatedChunk = DatedChunk
  { dcDate :: !Day
  , dcContent :: !Text
  }
  deriving stock (Eq, Show)

newtype UndatedChunk = UndatedChunk {getUndatedChunk :: Text}
  deriving newtype (Eq, Show, Semigroup, Monoid)

data Chunk = ChunkDatedChunk !DatedChunk | ChunkUndatedChunk !UndatedChunk
  deriving stock (Eq, Show)

newtype Journal = Journal {getJournal :: [Chunk]}
  deriving newtype (Eq, Show, Semigroup, Monoid)

type Parser = Parsec Void Text

newlineOrEof :: Parser Text
newlineOrEof = (toText . one @Text <$> newline) <|> (eof $> "")

anyLineP :: Parser Text
anyLineP = do
  (line, eol) <-
    manyTill_
      anySingle
      newlineOrEof
  return $ toText line <> eol

commentLineP :: Parser Text
commentLineP = label "a comment" $ do
  start <- toText . one @Text <$> single ';'
  comment <- takeWhileP Nothing (/= '\n')
  nl <- newlineOrEof
  return $ start <> comment <> nl

dateP :: Parser Day
dateP = label "a date" $ do
  dateString <- someTill printChar spaceChar
  label "date string" $
    parseTimeM True defaultTimeLocale "%Y-%m-%d" dateString
      <|> parseTimeM True defaultTimeLocale "%Y/%m/%d" dateString

datedLineP :: Parser Day
datedLineP = label "a line with a date" $ do
  let directivePrefixP = letterChar >> some spaceChar
  void $ optional directivePrefixP
  day <- dateP
  void $
    manyTill
      printChar
      newlineOrEof
  return day

datedChunkP :: Parser DatedChunk
datedChunkP = label "a dated chunk (a transaction or a directive)" $ do
  (comments, datedLineString, day) <- try start
  ils <- fold <$> many indentedLineP
  return $ DatedChunk day (comments <> datedLineString <> ils)
 where
  start = do
    comments <- fold <$> many commentLineP
    (datedLineString, day) <- match datedLineP
    return (comments, datedLineString, day)
  indentedLineP = liftA2 (<>) (one @Text <$> char ' ') anyLineP

undatedChunkP :: Parser UndatedChunk
undatedChunkP = UndatedChunk . fold <$> manyTill anyLineP ((void . try . lookAhead $ datedChunkP) <|> void eof)

chunkP :: Parser Chunk
chunkP = (ChunkDatedChunk <$> datedChunkP) <|> (ChunkUndatedChunk <$> undatedChunkP)

journalP :: Parser Journal
journalP = Journal <$> manyTill chunkP eof

parseJournal :: Text -> Either Text Journal
parseJournal =
  first
    (("Could not parse the journal.\n" <>) . toText . errorBundlePretty)
    . parse journalP ""
