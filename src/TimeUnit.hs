module TimeUnit where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (Reader, ReaderT(..), ask, runReader)
import qualified Data.Time.Clock as T
import qualified Data.Time.LocalTime as T

data TimeUnit = Hour
              | HalfHour
              | QuarterHour

type TimeChunk = (Int, TimeUnit)

type Timestamp = T.UTCTime

data TimeConfig = TimeConfig { unit :: TimeUnit
                             , timeZone :: T.TimeZone
                             }

localTime :: ReaderT TimeConfig IO T.LocalTime
localTime = do
    conf <- ask
    liftIO $ T.utcToLocalTime (timeZone conf) <$> T.getCurrentTime

localToChunk :: T.LocalTime -> Reader TimeConfig TimeChunk
localToChunk time = (\conf -> (computeValue conf, unit conf)) <$> ask
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
    chunk <- ReaderT $ return . (runReader $ localToChunk time)
    return chunk
