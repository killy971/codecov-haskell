-- |
-- Module:      Trace.Hpc.Codecov.Types
-- Copyright:   (c) 2014 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Types to represent hpc code coverage data.

module Trace.Hpc.Codecov.Types where

import Network.Curl
import Trace.Hpc.Mix

type CoverageEntry = (
    [MixEntry], -- mix entries
    [Integer],  -- tix values
    [String])   -- entry source code

data Hit = Full
         | Partial
         | None
         | Irrelevant
    deriving (Eq, Show)

type Lix = [Hit]

-- | Result to the POST request to codecov.io
data PostResult =
    PostSuccess URLString URLString -- ^ Codecov job url and wait url
  | PostFailure String              -- ^ error message
