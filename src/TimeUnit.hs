{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module TimeUnit
    ( TimeUnit(..)
    , timeSlots
    , minuteMarks
    , TimeSlot(..)
    , TimestampUtc
    , TimeConfig
    , unit
    , timeSlotToTimestamp
    , currentTimeSlot
    ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (Reader, ReaderT, ask, asks)
import qualified Data.Time.Calendar as T (Day)
import qualified Data.Time.Clock as T
import qualified Data.Time.LocalTime as T
import qualified Lens.Micro.Platform as LM (makeLenses, view)

import qualified Util as U (transformReader)

data TimeUnit = Hour
              | HalfHour
              | QuarterHour
              deriving (Eq, Show)

unitMultiplier :: TimeUnit -> Int
unitMultiplier =
    \case
        Hour -> 1
        HalfHour -> 2
        QuarterHour -> 4

newtype TimeSlot = TimeSlot { getTimeSlot :: (Int, TimeUnit) }
    deriving Eq

data TimestampUtc = TimestampUtc { dayUtc :: T.Day
                                 , timeSlotUtc :: TimeSlot
                                 }

data TimeConfig = TimeConfig { _unit :: TimeUnit
                             , _timeZone :: T.TimeZone
                             }
LM.makeLenses ''TimeConfig

instance Show TimeSlot where
  show (TimeSlot (x, unit')) = hours ++ ":" ++ minutes
    where
      hours :: String
      hours = show $ x `div` unitMultiplier unit'
      minutes :: String
      minutes = minuteMarksFor unit' !! (x `mod` unitMultiplier unit')

timeSlots :: Reader TimeConfig [TimeSlot]
timeSlots = timeSlotsFor <$> LM.view unit
  where
    timeSlotsFor :: TimeUnit -> [TimeSlot]
    timeSlotsFor unit' =
        (\x -> TimeSlot (x, unit')) <$> [0..unitMultiplier unit' * 24 - 1]

minuteMarksFor :: TimeUnit -> [String]
minuteMarksFor Hour = ["00"]
minuteMarksFor HalfHour = ["00", "30"]
minuteMarksFor QuarterHour = ["00", "15", "30", "45"]

minuteMarks :: Reader TimeConfig [String]
minuteMarks = minuteMarksFor <$> LM.view unit

timeSlotToTimestamp :: TimeSlot -> ReaderT TimeConfig IO TimestampUtc
timeSlotToTimestamp slot = dayToTimestamp <$> localToday
  where
    dayToTimestamp :: T.Day -> TimestampUtc
    dayToTimestamp day = TimestampUtc { dayUtc = day
                                      , timeSlotUtc = slot
                                      }

localTime :: ReaderT TimeConfig IO T.LocalTime
localTime = do
    tz <- LM.view timeZone
    liftIO $ T.utcToLocalTime tz <$> T.getCurrentTime

localToday :: ReaderT TimeConfig IO T.Day
localToday = T.localDay <$> localTime

timeOfDayToSlot :: T.TimeOfDay -> Reader TimeConfig TimeSlot
timeOfDayToSlot tod = computeSlot <$> LM.view unit
    where
        computeUnits units =
            case units of
              Hour -> T.todHour tod
              HalfHour -> T.todHour tod * 2 + (T.todMin tod `div` 30)
              QuarterHour -> T.todHour tod * 4 + (T.todMin tod `div` 15)
        computeSlot :: TimeUnit -> TimeSlot
        computeSlot units = TimeSlot (computeUnits units, units)

currentTimeSlot :: ReaderT TimeConfig IO TimeSlot
currentTimeSlot =
    localTime >>= U.transformReader . timeOfDayToSlot . T.localTimeOfDay
