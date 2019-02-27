module Env where

import qualified Config as C (Config)
import qualified State as S (State)
import qualified UserData as UD (UserData)

data Env = Env { config :: C.Config
               , state :: S.State
               , userData :: UD.UserData
               }
