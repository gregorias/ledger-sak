module LedgerDiffMain (main) where

import LedgerDiff (diff)
import Options.Applicative (Parser, execParser, fullDesc, header, helper, info, metavar, progDesc, strArgument)
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
  putText =<< diff leftPath rightPath
 where
  opts =
    info
      (filePathsP <**> helper)
      ( fullDesc
          <> progDesc "Runs a smart chronological diff on two Ledger files."
          <> header "ledgerdiff"
      )
