{-# LANGUAGE TemplateHaskell #-}

module Tag where

import qualified Data.Aeson.TH as A (defaultOptions, deriveJSON)
import qualified Data.Text as T (Text)

import qualified TimeUnit as TU

data RawTag = RawTag { tagId      :: Integer
                     , text       :: T.Text
                     , categoryId :: Integer
                     , time       :: TU.TimestampUtc
                     }
$(A.deriveJSON A.defaultOptions ''RawTag)

data UiTag = UiTag { raw :: RawTag }

