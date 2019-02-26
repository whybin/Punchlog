{-# LANGUAGE TemplateHaskell #-}

module Parser.Tag where

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A (defaultOptions, deriveJSON)
import qualified Data.HashMap.Lazy as H (toList)
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.Read as T (decimal)
import qualified Data.Time.Calendar as TC (Day(..))

import Tag (AllTags(..), DayTags, RawTag)

$(A.deriveJSON A.defaultOptions ''RawTag)

kvFromDaily :: A.KeyValue kv => DayTags -> kv
kvFromDaily (day, tags) = (T.pack . show $ day) .= tags

instance A.ToJSON AllTags where
    toJSON = A.object . map kvFromDaily . getAllTags
    toEncoding = A.pairs . mconcat . map kvFromDaily . getAllTags

instance A.FromJSON AllTags where
    parseJSON = A.withObject "AllTags" $
        fmap AllTags . traverse toDaily . H.toList
            where toDaily (dayStr, dataStr) =
                    case T.decimal dayStr of
                      Left _ ->
                          fail $ "Failed to parse day: " ++ T.unpack dayStr
                      Right (day, _) ->
                          sequenceA ( TC.ModifiedJulianDay day
                                    , A.parseJSON dataStr
                                    )
