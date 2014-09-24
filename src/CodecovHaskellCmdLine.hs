{-# LANGUAGE DeriveDataTypeable #-}

module CodecovHaskellCmdLine where

import Data.List
import Data.Version (Version(..))
import Paths_codecov_haskell (version)
import System.Console.CmdArgs
import Trace.Hpc.Codecov.Types

data CodecovHaskellArgs = CmdMain
    { excludeDirs   :: [String]
    , testSuites    :: [String]
    , displayReport :: Bool
    , printResponse :: Bool
    , dontSend      :: Bool
    , coverageMode  :: CoverageMode
    } deriving (Data, Show, Typeable)

codecovHaskellArgs :: CodecovHaskellArgs
codecovHaskellArgs = CmdMain
    { excludeDirs   = []                &= explicit &= typDir     &= name "exclude-dir"    &= help "Exclude sources files under the matching directory from the coverage report"
    , displayReport = False             &= explicit               &= name "display-report" &= help "Display the json code coverage report that will be sent to codecov.io"
    , printResponse = False             &= explicit               &= name "print-response" &= help "Prints the json reponse received from codecov.io"
    , dontSend      = False             &= explicit               &= name "dont-send"      &= help "Do not send the report to codecov.io"
    , coverageMode  = AllowPartialLines &= explicit &= typ "MODE" &= name "coverage-mode"  &= help "Coverage conversion mode: AllowPartialLines (default), StrictlyFullLines"
    , testSuites    = []                &= typ "TEST-SUITE" &= args
    } &= summary ("codecov-haskell-" ++ versionString version ++ ", (C) Guillaume Nargeot 2014")
      &= program "codecov-haskell"
    where versionString = intercalate "." . map show . versionBranch
