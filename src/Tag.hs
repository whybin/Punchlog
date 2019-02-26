module Tag
    ( AllTags(..)
    , DayTags
    , RawTag
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

type DayTags = (TC.Day, [RawTag])

newtype AllTags = AllTags { getAllTags :: [DayTags] }

-- UI tags

newtype UiTag = UiTag { raw :: RawTag }
