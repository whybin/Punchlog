{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module App
    ( runApp
    ) where

import Control.Monad (void)

import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative (Attribute((:=)))
import qualified GI.Gtk.Declarative as GD
  ( Widget(..)
  , bin
  , container
  , on
  , widget
  )
import qualified GI.Gtk.Declarative.App.Simple as GD

import qualified Env as E (Env(..))
import qualified Config as C (Config)
import qualified State as S (State(..))
import qualified UserData as UD (UserData)

type AppState = E.Env

data Event = Quit

update :: AppState -> Event -> GD.Transition AppState Event
update st Quit = GD.Exit

view :: AppState -> GD.AppView Gtk.Window Event
view st = GD.bin Gtk.Window [ #title := "Punchlog"
                            , GD.on #deleteEvent (const (True, Quit))
                            ] $ dayView st

dayView :: AppState -> GD.Widget Event
dayView st = GD.bin Gtk.Viewport [] $
    GD.container Gtk.Box [ #orientation := Gtk.OrientationVertical ]
        [ GD.widget Gtk.Label [ #label := "Today"
                              , #xalign := 0
                              ]
        , GD.container Gtk.ListBox [] []
        ]

initialState :: C.Config -> UD.UserData -> AppState
initialState config userData =
    E.Env { E.config = config
          , E.state = S.State
          , E.userData = userData
          }

runApp :: C.Config -> UD.UserData -> IO ()
runApp config userData = void $
    GD.run GD.App { GD.update = update
                  , GD.view = view
                  , GD.inputs = []
                  , GD.initialState = initialState config userData
                  }
