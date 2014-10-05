module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.List
import           Data.Maybe
import           CodecovHaskellCmdLine
import           System.Console.CmdArgs
import           System.Environment (getEnv, getEnvironment)
import           System.Exit (exitFailure, exitSuccess)
import           Trace.Hpc.Codecov
import           Trace.Hpc.Codecov.Config (Config(Config))
import           Trace.Hpc.Codecov.Curl

baseUrlApiV1 :: String
baseUrlApiV1 = "https://codecov.io/upload/v1"

getUrlApiV1 :: IO String
getUrlApiV1 = do
    env <- getEnvironment
    case snd <$> find (isJust . flip lookup env . fst) ciEnvVars of
        Just ((idParamName, idParamEnvVar), commitEnvVar, branchEnvVar) -> do
            idParamValue <- getEnv idParamEnvVar
            commit <- getEnv commitEnvVar
            branch <- getEnv branchEnvVar
            return $ baseUrlApiV1 ++ "?" ++ idParamName ++ "=" ++ idParamValue ++ "&commit=" ++ commit ++ "&branch=" ++ branch
        _ -> error "Unsupported CI service."
    where ciEnvVars = [
           ("TRAVIS", (("travis_job_id", "TRAVIS_JOB_ID"), "TRAVIS_COMMIT", "TRAVIS_BRANCH"))]

getConfig :: CodecovHaskellArgs -> Maybe Config
getConfig cha = case testSuites cha of
    []             -> Nothing
    testSuiteNames -> Just $ Config testSuiteNames (excludeDirs cha)

main :: IO ()
main = do
    cha <- cmdArgs codecovHaskellArgs
    case getConfig cha of
        Nothing -> putStrLn "Please specify a target test suite name" >> exitSuccess
        Just config -> do
            codecovJson <- generateCodecovFromTix config
            when (displayReport cha) $ BSL.putStrLn $ encode codecovJson
            unless (dontSend cha) $ do
                apiUrl <- getUrlApiV1
                response <- postJson (BSL.unpack $ encode codecovJson) apiUrl (printResponse cha)
                case response of
                    PostSuccess url totalCoverage -> do
                        putStrLn ("URL: " ++ url)
                        putStrLn ("Coverage: " ++ totalCoverage)
                    PostFailure msg -> putStrLn ("Error: " ++ msg) >> exitFailure
