module UserData
    ( dataFromRaw
    , rawFromData
    ) where

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B (ByteString)
import qualified Data.HashMap.Lazy as H (toList)
import qualified Data.Text as T (pack, unpack)
import qualified Data.Text.Read as T (decimal)
import qualified Data.Time.Calendar as TC (Day(..))

import qualified Tag as Tg (RawTag)

type DailyData = (TC.Day, [Tg.RawTag])

kvFromDaily :: A.KeyValue kv => DailyData -> kv
kvFromDaily (day, tags) = (T.pack . show $ day) .= tags

newtype AllData = AllData { getAllData :: [DailyData] }

instance A.ToJSON AllData where
    toJSON = A.object . map kvFromDaily . getAllData
    toEncoding = A.pairs . mconcat . map kvFromDaily . getAllData

instance A.FromJSON AllData where
    parseJSON = A.withObject "AllData" $
        fmap AllData . traverse toDaily . H.toList
            where toDaily (dayStr, dataStr) =
                case T.decimal dayStr of
                  Left _ -> fail $ "Failed to parse day: " ++ T.unpack dayStr
                  Right (day, _) -> sequenceA ( TC.ModifiedJulianDay day
                                              , A.parseJSON dataStr
                                              )
rawFromData :: AllData -> B.ByteString
rawFromData = A.encode

dataFromRaw :: B.ByteString -> Maybe AllData
dataFromRaw = A.decode
