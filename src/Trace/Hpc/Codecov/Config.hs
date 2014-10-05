module Trace.Hpc.Codecov.Config where

data Config = Config {
    testSuites   :: ![String],
    excludedDirs :: ![FilePath]
    }
