module Main where

import System.IO (IOMode(ReadMode), withFile)
import System.IO.Error (IOError, catchIOError, ioError, isDoesNotExistError)

import qualified Data.Yaml as Y (prettyPrintParseException)

import qualified Config as C (Config)
import qualified Parser.Config as PC (configFromFilePath)
import qualified Parser.UserData as PU
  ( dataFromFilePath
  , defaultData
  , defaultDataPath
  )
import qualified UserData as UD (UserData)

retrieveConfig :: IO C.Config
retrieveConfig =
    let configPath = "config.yml"
     in PC.configFromFilePath configPath
         >>= either (fail . Y.prettyPrintParseException) pure

retrieveData :: IO UD.UserData
retrieveData =
    catchIOError dataFromFile handleException
        where
            dataPath = PU.defaultDataPath
            dataFromFile :: IO UD.UserData
            dataFromFile = PU.dataFromFilePath dataPath
                >>= maybe (fail "User data file cannot be parsed") pure
            handleException :: IOError -> IO UD.UserData
            handleException e = if isDoesNotExistError e
                                   then pure PU.defaultData
                                   else ioError e

main :: IO ()
main = someFunc
