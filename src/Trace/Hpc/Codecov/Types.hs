{-# LANGUAGE DeriveDataTypeable #-}

-- |
-- Module:      Trace.Hpc.Codecov.Types
-- Copyright:   (c) 2014 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Types to represent hpc code coverage data.

module Trace.Hpc.Codecov.Types where

import Data.Data
import Network.Curl
import System.Console.CmdArgs.Default
import Trace.Hpc.Mix

type CoverageEntry = (
    MixEntry, -- mix entry
    Integer,  -- tix value
    [String])   -- entry source code

data Hit = Full
         | Partial
         | None
         | Irrelevant
    deriving (Eq, Show)

type Lix = [Hit]

instance Default CoverageMode where
    def = AllowPartialLines

data CoverageMode = StrictlyFullLines
                  | AllowPartialLines
    deriving (Data, Eq, Show, Typeable)

-- | Result to the POST request to codecov.io
data PostResult =
    PostSuccess URLString String -- ^ Codecov job url and total coverage percentage
  | PostFailure String           -- ^ error message
