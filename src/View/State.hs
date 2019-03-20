module View.State
    ( Day(..)
    , calendarDay
    , ViewState(..)
    ) where

import qualified Data.Time.Calendar as T (Day)
import qualified Data.Time.Clock as T (UTCTime(..))

data Day = Today T.UTCTime | PriorDay T.Day

instance Show Day where
  show (Today _) = "Today"
  show (PriorDay day) = show day

calendarDay :: Day -> T.Day
calendarDay (Today day) = T.utctDay day
calendarDay (PriorDay day) = day

data ViewState = Schedule Day | Settings
