{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- |
-- Module:      Trace.Hpc.Codecov.Curl
-- Copyright:   (c) 2014-2015 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Functions for sending coverage report files over http.

module Trace.Hpc.Codecov.Curl ( postJson, readCoverageResult, PostResult (..) ) where

import           Control.Applicative
import           Control.Monad
import           Control.Retry
import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Maybe
import           Network.Curl
import           Trace.Hpc.Codecov.Types

parseResponse :: CurlResponse -> PostResult
parseResponse r = case respCurlCode r of
    CurlOK -> PostSuccess (getField "url") (getField "wait_url")
    _      -> PostFailure $ getField "message"
    where getField fieldName = fromJust $ mGetField fieldName
          mGetField fieldName = do
              result <- decode $ LBS.pack (respBody r)
              parseMaybe (.: fieldName) result

-- | Send json coverage report over HTTP using POST request
postJson :: String        -- ^ json coverage report
         -> URLString     -- ^ target url
         -> Bool          -- ^ print response body if true
         -> IO PostResult -- ^ POST request result
postJson jsonCoverage url printResponse = do
    h <- initialize
    setopt h (CurlPost True)
    setopt h (CurlVerbose True)
    setopt h (CurlURL url)
    setopt h (CurlHttpHeaders ["Content-Type: application/json"])
    setopt h (CurlPostFields [jsonCoverage])
    r <- perform_with_response_ h
    when printResponse $ putStrLn $ respBody r
    return $ parseResponse r

-- | Exponential retry policy of 10 seconds initial delay, up to 5 times
expRetryPolicy :: RetryPolicy
expRetryPolicy = exponentialBackoff (10 * 1000 * 1000) <> limitRetries 3

performWithRetry :: IO (Maybe a) -> IO (Maybe a)
performWithRetry = retrying expRetryPolicy isNothingM
    where isNothingM _ = return . isNothing

extractCoverage :: String -> Maybe String
extractCoverage rBody = (++ "%") . show <$> (getField "coverage" :: Maybe Integer)
    where getField fieldName = do
              result <- decode $ LBS.pack rBody
              parseMaybe (.: fieldName) result

-- | Read the coveraege result page from coveralls.io
readCoverageResult :: URLString         -- ^ target url
                   -> Bool              -- ^ print json response if true
                   -> IO (Maybe String) -- ^ coverage result
readCoverageResult url printResponse =
    performWithRetry readAction
    where readAction = do
          response <- curlGetString url curlOptions
          when printResponse $ putStrLn $ snd response
          return $ case response of
              (CurlOK, body) -> extractCoverage body
              _ -> Nothing
          where curlOptions = [
                    CurlTimeout 60,
                    CurlConnectTimeout 60,
                    CurlVerbose True,
                    CurlFollowLocation True]
