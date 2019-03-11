module UserData
    ( UserData(..)
    ) where

import qualified Tag.Class as Tag (AllTags)

newtype UserData = UserData { tags :: Tag.AllTags
                            }
