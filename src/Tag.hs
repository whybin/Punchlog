{-# LANGUAGE TemplateHaskell #-}

module Tag
    ( AllTags
    ) where

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.TH as A (defaultOptions, deriveJSON)
import qualified Data.ByteString.Lazy as B (ByteString)
import qualified Data.HashMap.Lazy as H (toList)
import qualified Data.Text as T (Text, pack, unpack)
import qualified Data.Text.Read as T (decimal)
import qualified Data.Time.Calendar as TC (Day(..))

import qualified TimeUnit as TU

data RawTag = RawTag { tagId      :: Integer
                     , text       :: T.Text
                     , categoryId :: Integer
                     , time       :: TU.TimestampUtc
                     }
$(A.deriveJSON A.defaultOptions ''RawTag)

-- Aggregate tag data

type DayTags = (TC.Day, [RawTag])

kvFromDaily :: A.KeyValue kv => DayTags -> kv
kvFromDaily (day, tags) = (T.pack . show $ day) .= tags

newtype AllTags = AllTags { getAllTags :: [DayTags] }

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

-- UI tags

newtype UiTag = UiTag { raw :: RawTag }
