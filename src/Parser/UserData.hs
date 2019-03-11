{-# LANGUAGE TemplateHaskell #-}

module Parser.UserData
    ( defaultDataPath
    , dataFromFilePath
    , defaultData
    ) where

import System.IO (FilePath)

import qualified Data.Aeson as A (decodeFileStrict)
import qualified Data.Aeson.TH as A (defaultOptions, deriveJSON)

import Parser.Tag
import qualified Tag.Class as Tag (AllTags(..))
import UserData

$(A.deriveJSON A.defaultOptions ''UserData)

defaultDataPath :: FilePath
defaultDataPath = "data.json"

dataFromFilePath :: FilePath -> IO (Maybe UserData)
dataFromFilePath = A.decodeFileStrict

defaultData :: UserData
defaultData = UserData { tags = Tag.AllTags [] }
