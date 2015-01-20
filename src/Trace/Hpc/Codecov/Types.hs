-- |
-- Module:      Trace.Hpc.Codecov.Types
-- Copyright:   (c) 2014-2015 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Types to represent hpc code coverage data.

module Trace.Hpc.Codecov.Types where

import Data.Aeson
import Network.Curl
import Trace.Hpc.Mix
import Trace.Hpc.Util

-- single file coverage data in the format defined by codecov.io
type SimpleCoverage = [Value]

type LixConverter = Lix -> SimpleCoverage

type CoverageEntry = (
    HpcPos,      -- position
    ([BoxLabel], -- box labels
     [Integer],  -- tix values
     [String]))  -- entry source code

data Hit = Full
         | Partial
         | None
         | Irrelevant
    deriving (Eq, Show)

type ExprHit = (Hit, (Int, Int))

type Lix = [Maybe [ExprHit]]

-- | Result to the POST request to codecov.io
data PostResult =
    PostSuccess URLString URLString -- ^ Codecov job url and wait url
  | PostFailure String              -- ^ error message
