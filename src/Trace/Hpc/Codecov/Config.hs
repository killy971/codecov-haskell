module Trace.Hpc.Codecov.Config where

import Trace.Hpc.Codecov.Types (CoverageMode)

data Config = Config {
    testSuites   :: ![String],
    excludedDirs :: ![FilePath],
    coverageMode :: !CoverageMode
    }
