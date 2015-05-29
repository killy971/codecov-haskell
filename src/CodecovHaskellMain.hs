module Main where

import           Control.Applicative
import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BSL
import           Data.List
import           Data.Maybe hiding (listToMaybe)
import           CodecovHaskellCmdLine
import           System.Console.CmdArgs
import           System.Environment (getEnv, getEnvironment)
import           System.Exit (exitFailure, exitSuccess)
import           Trace.Hpc.Codecov
import           Trace.Hpc.Codecov.Config (Config(Config))
import           Trace.Hpc.Codecov.Curl
import           Trace.Hpc.Codecov.Util

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
           ("TRAVIS", (("travis_job_id", "TRAVIS_JOB_ID"), "TRAVIS_COMMIT", "TRAVIS_BRANCH")),
           ("JENKINS_HOME", (("travis_job_id", "BUILD_NUMBER"), "GIT_COMMIT", "GIT_BRANCH"))]

getUrlWithToken :: String -> String -> Maybe String -> IO String
getUrlWithToken apiUrl _ Nothing = return $ apiUrl
getUrlWithToken apiUrl param (Just t) = return $ apiUrl ++ "&" ++ param ++ "=" ++ t

getConfig :: CodecovHaskellArgs -> Maybe Config
getConfig cha = Config (excludeDirs cha) <$> listToMaybe (testSuites cha)

main :: IO ()
main = do
    cha <- cmdArgs codecovHaskellArgs
    case getConfig cha of
        Nothing -> putStrLn "Please specify a target test suite name" >> exitSuccess
        Just config -> do
            let apiToken = (token cha)
            codecovJson <- generateCodecovFromTix config
            when (displayReport cha) $ BSL.putStrLn $ encode codecovJson
            unless (dontSend cha) $ do
                apiUrl <- getUrlApiV1
                fullUrl <- getUrlWithToken apiUrl "token" apiToken
                response <- postJson (BSL.unpack $ encode codecovJson) fullUrl (printResponse cha)
                case response of
                    PostSuccess url _ -> do
                        responseUrl <- getUrlWithToken url "access_token" apiToken
                        putStrLn ("URL: " ++ responseUrl)
                        -- wait 10 seconds until the page is available
                        threadDelay (10 * 1000000)
                        coverageResult <- readCoverageResult responseUrl (printResponse cha)
                        case coverageResult of
                            Just totalCoverage -> putStrLn ("Coverage: " ++ totalCoverage) >> exitSuccess
                            Nothing -> putStrLn "Failed to read total coverage" >> exitSuccess
                    PostFailure msg -> putStrLn ("Error: " ++ msg) >> exitFailure
