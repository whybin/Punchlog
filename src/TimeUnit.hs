{-# LANGUAGE TemplateHaskell #-}

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
import qualified Lens.Micro.Platform as LM (makeLenses, view)

import qualified Util as U (liftReader)

data TimeUnit = Hour
              | HalfHour
              | QuarterHour
              deriving Show

type TimeSlot = (Int, TimeUnit)

data TimestampUtc = TimestampUTC { dayUtc :: T.Day
                                 , timeSlotUtc :: TimeSlot
                                 }

data TimeConfig = TimeConfig { _unit :: TimeUnit
                             , _timeZone :: T.TimeZone
                             }
LM.makeLenses ''TimeConfig

minuteMarksFor :: TimeUnit -> [String]
minuteMarksFor Hour = ["00"]
minuteMarksFor HalfHour = ["00", "30"]
minuteMarksFor QuarterHour = ["00", "15", "30", "45"]

minuteMarks :: Reader TimeConfig [String]
minuteMarks = minuteMarksFor <$> LM.view unit

localTime :: ReaderT TimeConfig IO T.LocalTime
localTime = do
    tz <- LM.view timeZone
    liftIO $ T.utcToLocalTime tz <$> T.getCurrentTime


timeOfDayToSlot :: T.TimeOfDay -> Reader TimeConfig TimeSlot
timeOfDayToSlot tod = computeSlot <$> LM.view unit
    where
        computeUnits units =
            case units of
              Hour -> T.todHour tod
              HalfHour -> T.todHour tod * 2 + (T.todMin tod `div` 30)
              QuarterHour -> T.todHour tod * 4 + (T.todMin tod `div` 15)
        computeSlot :: TimeUnit -> TimeSlot
        computeSlot units = (computeUnits units, units)

currentTimeSlot :: ReaderT TimeConfig IO TimeSlot
currentTimeSlot =
    localTime >>= U.liftReader . timeOfDayToSlot . T.localTimeOfDay
