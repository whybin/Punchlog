module Config
    ( Config
    ) where

import qualified TimeUnit as TU (TimeConfig)

newtype Config = Config { timeConfig :: TU.TimeConfig }
