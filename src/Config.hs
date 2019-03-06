{-# LANGUAGE TemplateHaskell #-}

module Config
    ( Config(..)
    , timeConfig
    ) where

import qualified Lens.Micro.Platform as LM (makeLenses)

import qualified TimeUnit as TU (TimeConfig)

newtype Config = Config { _timeConfig :: TU.TimeConfig }
LM.makeLenses ''Config
