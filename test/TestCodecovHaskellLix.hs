{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module TestCodecovHaskellLix where

import Test.HUnit
import Trace.Hpc.Codecov.Lix
import Trace.Hpc.Codecov.Types

testToHit = "toHit" ~: [
    Irrelevant @=? toHit [],
    None       @=? toHit [False],
    None       @=? toHit [False, False],
    Partial    @=? toHit [False, True],
    Partial    @=? toHit [True, False],
    Partial    @=? toHit [False, False, True],
    Partial    @=? toHit [False, True, False],
    Partial    @=? toHit [True, False, False],
    Full       @=? toHit [True],
    Full       @=? toHit [True, True]]

testLix = "Lix" ~: [testToHit]
