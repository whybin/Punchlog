{-# LANGUAGE LambdaCase #-}

module View.Mapping where

import qualified Env as E (AppEnv)
import qualified View.Schedule as VS (scheduleView)
import qualified View.State as V (ViewState(..))
import qualified View.Type as V (View)

getView :: V.ViewState -> E.AppEnv V.View
getView = \case
            V.Schedule -> VS.scheduleView
