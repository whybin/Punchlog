{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module App
    ( runApp
    ) where

import Control.Monad (void)
import Control.Monad.Reader (runReader)

import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative (Attribute((:=)))
import qualified GI.Gtk.Declarative as GD
import qualified GI.Gtk.Declarative.App.Simple as GD
import qualified Lens.Micro.Platform as LM (view)

import qualified Env as E
import qualified Event as Ev (Event(..), IsEvent(..))
import qualified Config as C (Config)
import qualified UserData as UD (UserData)
import qualified View.State as V (ViewState(..))
import qualified View.Type as V (View)
import qualified View.Mapping as V (getView)

type AppState = E.Env

data Event = Quit
           | Noop

instance Ev.IsEvent Event where
  update st Quit = GD.Exit
  update st Noop = GD.Transition st $ pure Nothing

update :: E.Env -> Ev.Event -> GD.Transition E.Env Ev.Event
update st (Ev.Event ev) = Ev.update st ev

view :: AppState -> GD.AppView Gtk.Window Ev.Event
view st = GD.bin Gtk.Window [ #title := "Punchlog"
                            , #defaultWidth := 600
                            , #defaultHeight := 640
                            , GD.on #deleteEvent (const (True, Ev.Event Quit))
                            ]
            $ (runReader readView) st

readView :: E.AppEnv V.View
readView = LM.view (E.state . E.view) >>= V.getView

initialState :: C.Config -> UD.UserData -> AppState
initialState config userData =
    E.Env { E._config = config
          , E._state = E.State { E._view = V.Schedule
                               , E._creatingTag = Nothing
                               }
          , E._userData = userData
          }

runApp :: C.Config -> UD.UserData -> IO ()
runApp config userData = void $
    GD.run GD.App { GD.update = update
                  , GD.view = view
                  , GD.inputs = []
                  , GD.initialState = initialState config userData
                  }
