module TimeUnit
    ( TimeUnit
    , TimestampUtc
    , TimeConfig
    ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (Reader, ReaderT, ask, asks)
import qualified Data.Time.Calendar as T (Day)
import qualified Data.Time.Clock as T
import qualified Data.Time.LocalTime as T

import qualified Util as U (liftReader)

data TimeUnit = Hour
              | HalfHour
              | QuarterHour
              deriving Show

type TimeChunk = (Int, TimeUnit)

data TimestampUtc = TimestampUTC { dayUtc :: T.Day
                                 , timeChunkUtc :: TimeChunk
                                 }

data TimeConfig = TimeConfig { unit :: TimeUnit
                             , timeZone :: T.TimeZone
                             }

localTime :: ReaderT TimeConfig IO T.LocalTime
localTime = do
    conf <- ask
    liftIO $ T.utcToLocalTime (timeZone conf) <$> T.getCurrentTime

localToChunk :: T.LocalTime -> Reader TimeConfig TimeChunk
localToChunk time = asks $ \conf -> (computeValue conf, unit conf)
    where
        tod = T.localTimeOfDay time
        computeValue conf =
            case unit conf of
              Hour -> T.todHour tod
              HalfHour -> T.todHour tod * 2 + (T.todMin tod `div` 30)
              QuarterHour -> T.todHour tod * 4 + (T.todMin tod `div` 15)

currentTimeChunk :: ReaderT TimeConfig IO TimeChunk
currentTimeChunk = do
    time <- localTime
    U.liftReader $ localToChunk time
