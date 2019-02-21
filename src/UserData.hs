module UserData where

import Data.Aeson ((.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as B (ByteString)
import qualified Data.Text as T (pack)
import qualified Data.Time.Calendar as TC (Day)

import qualified Tag as Tg (RawTag)

type DailyData = (TC.Day, [Tg.RawTag])

kvFromDaily :: A.KeyValue kv => DailyData -> kv
kvFromDaily (day, tags) = (T.pack . show $ day) .= tags

newtype AllData = AllData { getAllData :: [DailyData] }

instance A.ToJSON AllData where
    toJSON = A.object . map kvFromDaily . getAllData
    toEncoding = A.pairs . mconcat . map kvFromDaily . getAllData

rawFromData :: AllData -> B.ByteString
rawFromData = A.encode
