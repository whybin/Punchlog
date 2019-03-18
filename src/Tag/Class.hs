module Tag.Class
    ( AllTags(..)
    , DayTags(..)
    , RawTag(..)
    ) where

import qualified Data.Text as T (Text)
import qualified Data.Time.Calendar as TC (Day)

import qualified TimeUnit as TU

data RawTag = RawTag { tagId      :: Integer
                     , text       :: T.Text
                     , categoryId :: Integer
                     , time       :: TU.TimestampUtc
                     }

-- Aggregate tag data

newtype DayTags = DayTags { getDayTags :: (TC.Day, [RawTag]) }

-- | Equality of DayTags is established by comparing the day referred to,
-- regardless of the value(s) of its tags
instance Eq DayTags where
  DayTags (d, _) == DayTags (d', _) = d == d'

instance Ord DayTags where
  compare (DayTags (d, _)) (DayTags (d', _)) = compare d d'

-- Collection of all tags by day in reverse chronological order
newtype AllTags = AllTags { getAllTags :: [DayTags] }

-- UI tags

newtype UiTag = UiTag { raw :: RawTag }
