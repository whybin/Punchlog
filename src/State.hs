module State where

import qualified View as V

data State = State { view :: Maybe V.View
                   }
