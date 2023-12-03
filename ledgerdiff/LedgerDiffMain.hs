module LedgerDiffMain (main) where

import Data.Text.IO qualified as T
import Data.Version (showVersion)
import LedgerDiff (diffLedgerIO)
import Options.Applicative (Parser, execParser, fullDesc, header, helper, info, metavar, progDesc, simpleVersioner, strArgument)
import Paths_ledger_sak (version)
import Relude

data FilePaths = FilePaths
  { _fpLeft :: FilePath
  , _fpRight :: FilePath
  }

filePathsP :: Parser FilePaths
filePathsP =
  FilePaths
    <$> strArgument (metavar "LEFT_FILE")
    <*> strArgument (metavar "RIGHT_FILE")

main :: IO ()
main = do
  (FilePaths leftPath rightPath) <- execParser opts
  eitherDiff <- diffLedgerIO leftPath rightPath
  either
    ( \err -> do
        T.hPutStr stderr err
        exitFailure
    )
    putText
    eitherDiff
 where
  opts =
    info
      (filePathsP <**> helper <**> simpleVersioner (showVersion version))
      ( fullDesc
          <> progDesc "Runs a smart chronological diff on two Ledger files."
          <> header "ledgerdiff"
      )
