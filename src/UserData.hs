{-# LANGUAGE TemplateHaskell #-}

module UserData
    ( UserData(..)
    , tags
    ) where

import qualified Lens.Micro.Platform as LM (makeLenses)

import qualified Tag.Class as Tag (AllTags)

newtype UserData = UserData { _tags :: Tag.AllTags
                            }
LM.makeLenses ''UserData
