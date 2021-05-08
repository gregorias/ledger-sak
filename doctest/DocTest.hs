module DocTest (
  main,
) where

import Relude
import Test.DocTest (doctest)

main :: IO ()
main =
  doctest
    [ "-XNoImplicitPrelude"
    , "-XDerivingStrategies"
    , "-XGeneralizedNewtypeDeriving"
    , "-XOverloadedLists"
    , "-XOverloadedStrings"
    , "-XTypeApplications"
    , "-XScopedTypeVariables"
    , "-isrc"
    , "src/LedgerDiff.hs"
    ]
