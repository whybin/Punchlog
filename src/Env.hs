{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Env
    ( Env(..)
    , AppEnv(..)
    ) where

import Control.Monad.Reader (Reader)

import qualified Config as C (Config)
import qualified State as S (State)
import qualified UserData as UD (UserData)

data Env = Env { config :: C.Config
               , state :: S.State
               , userData :: UD.UserData
               }

newtype AppEnv a = AppEnv { runAppEnv :: Reader Env a }
    deriving (Functor, Applicative, Monad)
