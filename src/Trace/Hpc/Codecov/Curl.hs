{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

-- |
-- Module:      Trace.Hpc.Codecov.Curl
-- Copyright:   (c) 2014 Guillaume Nargeot
-- License:     BSD3
-- Maintainer:  Guillaume Nargeot <guillaume+hackage@nargeot.com>
-- Stability:   experimental
--
-- Functions for sending coverage report files over http.

module Trace.Hpc.Codecov.Curl ( postJson, PostResult (..) ) where

import           Control.Monad
import           Data.Aeson
import           Data.Aeson.Types (parseMaybe)
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Maybe
import           Network.Curl
import           Trace.Hpc.Codecov.Types

parseResponse :: CurlResponse -> PostResult
parseResponse r = case respCurlCode r of
    CurlOK -> PostSuccess (getField "url") (show $ (getField "coverage" :: Integer))
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
