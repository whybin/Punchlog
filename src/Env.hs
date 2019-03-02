{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Env
    ( Env(..)
    , AppEnvT(..)
    , AppEnv
    , runAppEnv
    , liftToAppEnv
    ) where

import Control.Monad.Reader (ReaderT)
import Data.Functor.Identity (Identity)

import qualified Config as C (Config)
import qualified State as S (State)
import qualified UserData as UD (UserData)

data Env = Env { config :: C.Config
               , state :: S.State
               , userData :: UD.UserData
               }

newtype AppEnvT m a = AppEnvT { runAppEnvT :: ReaderT Env m a }
    deriving (Functor, Applicative, Monad)

type AppEnv a = AppEnvT Identity a

runAppEnv :: AppEnv a -> ReaderT Env Identity a
runAppEnv = runAppEnvT

liftToAppEnv :: ReaderT Env Identity a -> AppEnv a
liftToAppEnv = AppEnvT
