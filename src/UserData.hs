module UserData where

import qualified Tag (AllTags)

newtype UserData = UserData { tags :: Tag.AllTags
                            }
