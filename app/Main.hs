module Main where

import qualified Data.Yaml as Y (prettyPrintParseException)

import qualified Config as C (Config)
import qualified Parser.Config as PC (configFromFilePath)

retrieveConfig :: IO C.Config
retrieveConfig =
    let configPath = "config.yml"
     in PC.configFromFilePath configPath
         >>= either (fail . Y.prettyPrintParseException) pure

main :: IO ()
main = someFunc
