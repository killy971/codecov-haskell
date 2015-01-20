{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Trace.Hpc.Json
-- Copyright:   (c) 2014-2015 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Functions for generating the core coverage data in the json format defined by codecov.io

module Trace.Hpc.Codecov.Json ( defaultConverter ) where

import           Control.Arrow (first)
import           Data.Aeson
import           Data.Aeson.Types ()
import           Data.Maybe
import qualified Data.Vector as V
import           Trace.Hpc.Codecov.Types

convHit :: Hit -> Value
convHit hit = case hit of
    Full       -> Number 1
    Partial    -> Bool True
    None       -> Number 0
    Irrelevant -> Null

convArray :: [Value] -> Value
convArray = Array . V.fromList

convTuple :: (Value, (Int, Int)) -> Value
convTuple (hit, (start, end)) = convArray [toVal start, toVal end, hit]
    where toVal = Number . fromIntegral

convExpr :: [ExprHit] -> Value
convExpr = convArray . map (convTuple . first convHit)

defaultConverter :: LixConverter
defaultConverter = map (convLine . fmap convExpr)
    where convLine = fromMaybe Null

