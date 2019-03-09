{-# LANGUAGE TemplateHaskell #-}

module Env
    ( State(..)
    , view
    , creatingTag
    , Env(..)
    , config
    , state
    , userData
    , AppEnvT
    , AppEnv
    ) where

import Control.Monad.Reader (ReaderT)
import Data.Functor.Identity (Identity)

import qualified Lens.Micro.Platform as LM (makeLenses)

import qualified Config as C (Config)
import qualified TimeUnit as TU (TimeSlot)
import qualified UserData as UD (UserData)
import qualified View.State as V

data State = State { _view :: V.ViewState
                   , _creatingTag :: Maybe TU.TimeSlot
                   }

data Env = Env { _config :: C.Config
               , _state :: State
               , _userData :: UD.UserData
               }

type AppEnvT m a = ReaderT Env m a
type AppEnv a = AppEnvT Identity a

LM.makeLenses ''State
LM.makeLenses ''Env
