module Tag where

import qualified Data.Text as T (Text)

import qualified TimeUnit as TU

data RawTag = RawTag { tagId      :: Integer
                     , text       :: T.Text
                     , categoryId :: Integer
                     , time       :: TU.TimeChunk
                     }
data UiTag = UiTag { raw :: RawTag }

