{-# LANGUAGE TemplateHaskell #-}

module Parser.Config
    ( configFromFilePath
    ) where

import System.IO (FilePath)

import qualified Data.Aeson.TH as A (defaultOptions, deriveJSON)
import qualified Data.Yaml as Y (ParseException, decodeFileEither)

import Config
import Parser.TimeUnit

$(A.deriveJSON A.defaultOptions ''Config)

configFromFilePath :: FilePath -> IO (Either Y.ParseException Config)
configFromFilePath = Y.decodeFileEither
