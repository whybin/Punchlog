module TimeUnit
    ( TimeUnit(..)
    , minuteMarks
    , TimeSlot
    , TimestampUtc
    , TimeConfig
    , currentTimeSlot
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

minuteMarksFor :: TimeUnit -> [String]
minuteMarksFor Hour = ["00"]
minuteMarksFor HalfHour = ["00", "30"]
minuteMarksFor QuarterHour = ["00", "15", "30", "45"]

minuteMarks :: Reader TimeConfig [String]
minuteMarks = asks $ minuteMarksFor . unit

type TimeSlot = (Int, TimeUnit)

data TimestampUtc = TimestampUTC { dayUtc :: T.Day
                                 , timeSlotUtc :: TimeSlot
                                 }

data TimeConfig = TimeConfig { unit :: TimeUnit
                             , timeZone :: T.TimeZone
                             }

localTime :: ReaderT TimeConfig IO T.LocalTime
localTime = do
    conf <- ask
    liftIO $ T.utcToLocalTime (timeZone conf) <$> T.getCurrentTime

localToSlot :: T.LocalTime -> Reader TimeConfig TimeSlot
localToSlot time = asks $ \conf -> (computeValue conf, unit conf)
    where
        tod = T.localTimeOfDay time
        computeValue conf =
            case unit conf of
              Hour -> T.todHour tod
              HalfHour -> T.todHour tod * 2 + (T.todMin tod `div` 30)
              QuarterHour -> T.todHour tod * 4 + (T.todMin tod `div` 15)

timeOfDayToSlot :: T.TimeOfDay -> Reader TimeConfig TimeSlot
timeOfDayToSlot tod = asks computeSlot
    where
        computeUnits units =
            case units of
              Hour -> T.todHour tod
              HalfHour -> T.todHour tod * 2 + (T.todMin tod `div` 30)
              QuarterHour -> T.todHour tod * 4 + (T.todMin tod `div` 15)
        computeSlot conf = (computeUnits (unit conf), unit conf)

currentTimeSlot :: ReaderT TimeConfig IO TimeSlot
currentTimeSlot = do
    time <- localTime
    U.liftReader $ localToSlot time
