module Tag.New
    ( newTag
    ) where

import qualified Data.Text as T (Text)

import Tag.Class (RawTag(..))
import qualified TimeUnit as TU (TimestampUtc)

newTag :: TU.TimestampUtc -> T.Text -> RawTag
newTag timestamp text' =
    RawTag { tagId = 0
           , text = text'
           , categoryId = 0
           , time = timestamp
           }
