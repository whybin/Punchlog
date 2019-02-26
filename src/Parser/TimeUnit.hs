{-# LANGUAGE TemplateHaskell #-}

module Parser.TimeUnit where

import qualified Data.Aeson as A (FromJSON(..), ToJSON(..), Value(..), withText)
import qualified Data.Aeson.TH as A (defaultOptions, deriveJSON)
import qualified Data.Text as T (pack, unpack)
import qualified Data.Time.LocalTime as LT (TimeZone)

import TimeUnit (TimeConfig, TimestampUtc, TimeUnit)

$(A.deriveJSON A.defaultOptions ''TimeConfig)
$(A.deriveJSON A.defaultOptions ''TimestampUtc)
$(A.deriveJSON A.defaultOptions ''TimeUnit)

instance A.FromJSON LT.TimeZone where
    parseJSON = A.withText "TimeZone" (pure . read . T.unpack)

instance A.ToJSON LT.TimeZone where
    toJSON = A.String . T.pack . show
