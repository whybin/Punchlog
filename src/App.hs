{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module App
    ( runApp
    ) where

import Control.Monad (void)
import Control.Monad.Reader (runReader, withReader)

import qualified Data.Text as T (pack)
import qualified Data.Vector as V (fromList)
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative (Attribute((:=)))
import qualified GI.Gtk.Declarative as GD
import qualified GI.Gtk.Declarative.App.Simple as GD

import qualified Env as E (Env(..), AppEnv(..))
import qualified GtkDecl.Extra.FlowBox
import qualified Config as C (Config(..))
import qualified State as S (State(..))
import qualified TimeUnit as TU (minuteMarks)
import qualified UserData as UD (UserData)

type AppState = E.Env

data Event = Quit

update :: AppState -> Event -> GD.Transition AppState Event
update st Quit = GD.Exit

view :: AppState -> GD.AppView Gtk.Window Event
view st = GD.bin Gtk.Window [ #title := "Punchlog"
                            , GD.on #deleteEvent (const (True, Quit))
                            ]
            $ (runReader . E.runAppEnv $ dayView) st

readView :: E.AppEnv (GD.Widget Event)
readView = dayView

dayView :: E.AppEnv (GD.Widget Event)
dayView = do
    hoursView' <- hoursView
    pure $ GD.bin Gtk.Viewport [] $
        GD.container Gtk.Box [ #orientation := Gtk.OrientationVertical ]
            [ GD.widget Gtk.Label [ #label := "Today"
                                  , #xalign := 0
                                  ]
            , hoursView'
            ]
          where
            hoursView = do
                boxes <- timeBoxes
                pure $ GD.container Gtk.Box
                   [ #orientation := Gtk.OrientationVertical ]
                   [ GD.container Gtk.FlowBox
                       [ #selectionMode := Gtk.SelectionModeNone ]
                       boxes
                   ]
            timeLabels :: E.AppEnv [String]
            timeLabels = do
                minuteMarks <- E.AppEnv
                    $ withReader (C.timeConfig . E.config) TU.minuteMarks
                pure $ (++) <$> fmap show [0..23] <*> fmap (':':) minuteMarks
            timeToBox timeStr = GD.bin Gtk.FlowBoxChild []
                $ GD.widget Gtk.Label [#label := T.pack timeStr]
            timeBoxes = (V.fromList . fmap timeToBox) <$> timeLabels

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
