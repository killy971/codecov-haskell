-- |
-- Module:      Trace.Hpc.Codecov.Lix
-- Copyright:   (c) 2014-2015 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
-- Portability: portable
--
-- Functions for converting hpc output to line-based code coverage data.

module Trace.Hpc.Codecov.Lix where

import Data.List
import Data.Ord
import Prelude
import Trace.Hpc.Codecov.Types
import Trace.Hpc.Codecov.Util
import Trace.Hpc.Mix
import Trace.Hpc.Util

toHit :: [Bool] -> Hit
toHit []  = Irrelevant
toHit [x] = if x then Full else None
toHit xs
    | and xs    = Full
    | or xs     = Partial
    | otherwise = None

toExprHit :: CoverageEntry -> (Int, ExprHit)
toExprHit (pos, (_, counts, _)) = (line - 1, (hit, (start, end)))
    where (line, start, _, end) = fromHpcPos pos
          hit = toHit $ map (> 0) counts

isOtherwiseEntry :: CoverageEntry -> Bool
isOtherwiseEntry (_, (boxLabels, _, source)) =
    source == ["otherwise"] && boxLabels == otherwiseBoxLabels
        where otherwiseBoxLabels = [
               ExpBox False,
               BinBox GuardBinBox True,
               BinBox GuardBinBox False]

adjust :: CoverageEntry -> CoverageEntry
adjust coverageEntry@(pos, (boxLabels, tixs, source)) =
    if isOtherwiseEntry coverageEntry && any (> 0) tixs
    then (pos, (boxLabels, [1, 1, 1], source))
    else coverageEntry

-- | Convert hpc coverage entries into a line based coverage format
toLix :: Int             -- ^ Source line count
      -> [CoverageEntry] -- ^ Coverage entries
      -> Lix             -- ^ Line coverage
toLix lineCount entries = map listToMaybe (groupByIndex lineCount sortedExprHits)
    where sortedExprHits = sortBy (comparing fst) exprHits
          exprHits = map (toExprHit . adjust) entries
