{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module:      Trace.Hpc.Codecov
-- Copyright:   (c) 2014 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Functions for converting and sending hpc output to codecov.io.

module Trace.Hpc.Codecov ( generateCodecovFromTix ) where

import           Data.Aeson
import           Data.Aeson.Types ()
import           Data.Function
import           Data.List
import qualified Data.Map.Strict as M
import           System.Exit (exitFailure)
import           Trace.Hpc.Codecov.Config
import           Trace.Hpc.Codecov.Lix
import           Trace.Hpc.Codecov.Paths
import           Trace.Hpc.Codecov.Types
import           Trace.Hpc.Codecov.Util
import           Trace.Hpc.Mix
import           Trace.Hpc.Tix
import           Trace.Hpc.Util

type ModuleCoverageData = (
    String,    -- file source code
    Mix,       -- module index data
    [Integer]) -- tixs recorded by hpc

type TestSuiteCoverageData = M.Map FilePath ModuleCoverageData

-- single file coverage data in the format defined by codecov.io
type SimpleCoverage = [CoverageValue]

-- Is there a way to restrict this to only Number and Null?
type CoverageValue = Value

type LixConverter = Lix -> SimpleCoverage

defaultConverter :: LixConverter
defaultConverter = map $ \lix -> case lix of
    Full       -> Number 1
    Partial    -> Bool True
    None       -> Number 0
    Irrelevant -> Null

toSimpleCoverage :: LixConverter -> Int -> [CoverageEntry] -> SimpleCoverage
toSimpleCoverage convert lineCount = (:) Null . convert . toLix lineCount

getExprSource :: [String] -> MixEntry -> [String]
getExprSource source (hpcPos, _) = subSubSeq startCol endCol subLines
    where subLines = subSeq startLine endLine source
          startLine = startLine' - 1
          startCol = startCol' - 1
          (startLine', startCol', endLine, endCol) = fromHpcPos hpcPos

groupMixEntryTixs :: [(MixEntry, Integer, [String])] -> [CoverageEntry]
groupMixEntryTixs = map mergeOnLst3 . groupBy ((==) `on` fst . fst3)
    where mergeOnLst3 xxs@(x : _) = (map fst3 xxs, map snd3 xxs, trd3 x)
          mergeOnLst3 [] = error "mergeOnLst3 appliedTo empty list"

-- TODO possible renaming to "getModuleCoverage"
coverageToJson :: LixConverter -> ModuleCoverageData -> SimpleCoverage
coverageToJson converter (source, mix, tixs) = simpleCoverage
    where simpleCoverage = toSimpleCoverage converter lineCount mixEntriesTixs
          lineCount = length $ lines source
          mixEntriesTixs = groupMixEntryTixs mixEntryTixs
          mixEntryTixs = zip3 mixEntries tixs (map getExprSource' mixEntries)
          Mix _ _ _ _ mixEntries = mix
          getExprSource' = getExprSource $ lines source

toCodecovJson :: LixConverter -> TestSuiteCoverageData -> Value
toCodecovJson converter testSuiteCoverageData = object [
    "coverage" .= toJsonCoverageMap testSuiteCoverageData]
    where toJsonCoverageMap = M.map (coverageToJson converter)

mergeModuleCoverageData :: ModuleCoverageData -> ModuleCoverageData -> ModuleCoverageData
mergeModuleCoverageData (source, mix, tixs1) (_, _, tixs2) =
    (source, mix, zipWith (+) tixs1 tixs2)

mergeCoverageData :: [TestSuiteCoverageData] -> TestSuiteCoverageData
mergeCoverageData = foldr1 (M.unionWith mergeModuleCoverageData)

readMix' :: Config -> String -> TixModule -> IO Mix
readMix' config name tix = readMix (getMixPaths config name tix) $ Right tix

-- | Create a list of coverage data from the tix input
readCoverageData :: Config                   -- ^ codecov-haskell configuration 
                 -> String                   -- ^ test suite name
                 -> [String]                 -- ^ excluded source folders
                 -> IO TestSuiteCoverageData -- ^ coverage data list
readCoverageData config testSuiteName excludeDirPatterns = do
    tixPath <- getTixPath config testSuiteName
    mtix <- readTix tixPath
    case mtix of
        Nothing -> error ("Couldn't find the file " ++ tixPath) >> exitFailure
        Just (Tix tixs) -> do
            mixs <- mapM (readMix' config testSuiteName) tixs
            let files = map filePath mixs
            sources <- mapM readFile files
            let coverageDataList = zip4 files sources mixs (map tixModuleTixs tixs)
            let filteredCoverageDataList = filter sourceDirFilter coverageDataList
            return $ M.fromList $ map toFirstAndRest filteredCoverageDataList
            where filePath (Mix fp _ _ _ _) = fp
                  sourceDirFilter = not . matchAny excludeDirPatterns . fst4

-- | Generate codecov json formatted code coverage from hpc coverage data
generateCodecovFromTix :: Config   -- ^ codecov-haskell configuration
                       -> IO Value -- ^ code coverage result in json format
generateCodecovFromTix config = do
    testSuitesCoverages <- mapM (flip (readCoverageData config) excludedDirPatterns) testSuiteNames
    return $ toCodecovJson converter $ mergeCoverageData testSuitesCoverages
    where excludedDirPatterns = excludedDirs config
          testSuiteNames = testSuites config
          converter = defaultConverter
