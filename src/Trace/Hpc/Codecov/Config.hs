module Trace.Hpc.Codecov.Config where

data Config = Config {
    excludedDirs :: ![FilePath],
    testSuites   :: ![String]
    }
