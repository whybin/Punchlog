module Tag.Base
    ( newTag
    , addTag
    , tagsOnDay
    ) where

import qualified Data.Foldable as F (find)
import Data.Maybe (fromMaybe)

import qualified Data.Text as T (Text)
import qualified Data.Time.Calendar as TC (Day)

import Tag.Class (AllTags(..), DayTags(..), RawTag(..))
import qualified TimeUnit as TU (TimestampUtc(..))

newTag :: TU.TimestampUtc -> T.Text -> RawTag
newTag timestamp text' =
    RawTag { tagId = 0
           , text = text'
           , categoryId = 0
           , time = timestamp
           }

singletonDayTags :: RawTag -> DayTags
singletonDayTags tag =
    let day = TU.dayUtc $ time tag
     in DayTags (day, [tag])

addTag :: RawTag -> AllTags -> AllTags
addTag tag (AllTags all) = AllTags . insertByDay all $ singletonDayTags tag

insertByDay :: [DayTags] -> DayTags -> [DayTags]
insertByDay [] d = [d]
insertByDay ds d@(DayTags (day, tag)) =
    let (after, before) = span (> d) ds
        before' =
            case before of
              [] -> [d]
              b:bs | d < b -> d:b:bs
              b@(DayTags (_, tag')):bs | d == b ->
                  DayTags (day, tag ++ tag') : bs
     in after ++ before'

tagsOnDay :: AllTags -> TC.Day -> [RawTag]
tagsOnDay (AllTags allTags) day =
    fromMaybe [] . lookup day $ map getDayTags allTags
