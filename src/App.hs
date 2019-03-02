{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

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

import qualified Env as E (Env(..), AppEnv, liftToAppEnv, runAppEnv)
import Event (Event(..))
import qualified GtkDecl.Extra.FlowBox
import qualified Config as C (Config(..))
import qualified State as S (State(..))
import qualified TimeUnit as TU (minuteMarks)
import qualified Ui.TimeChunk as TC (onActivateTimeChunk)
import qualified UserData as UD (UserData)

type AppState = E.Env

update :: AppState -> Event -> GD.Transition AppState Event
update st Quit = GD.Exit
update st Noop = GD.Transition st $ pure Nothing

view :: AppState -> GD.AppView Gtk.Window Event
view st = GD.bin Gtk.Window [ #title := "Punchlog"
                            , #defaultWidth := 400
                            , #defaultHeight := 640
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
            [ GD.widget Gtk.Label [ #label := "Today", #xalign := 0 ]
            , hoursView'
            ]
  where
    hoursView :: E.AppEnv (_ Event)
    hoursView = do
        boxes <- timeBoxes
        onActivate <- TC.onActivateTimeChunk
        pure $ GD.BoxChild GD.defaultBoxChildProperties { GD.expand = True
                                                        , GD.fill = True
                                                        }
             $ GD.bin Gtk.ScrolledWindow []
             $ GD.container Gtk.ListBox [ GD.onM #rowSelected onActivate
                                        ] boxes
    timeLabels :: E.AppEnv [String]
    timeLabels = do
        minuteMarks <- E.liftToAppEnv
            $ withReader (C.timeConfig . E.config) TU.minuteMarks
        pure $ (++) <$> fmap show [0..23] <*> fmap (':':) minuteMarks
    timeToBox :: String -> _ Event
    timeToBox timeStr =
        GD.bin Gtk.ListBoxRow []
        $ GD.container Gtk.FlowBox [#selectionMode := Gtk.SelectionModeNone]
            [ GD.bin Gtk.FlowBoxChild []
                $ GD.widget Gtk.Label [#label := T.pack timeStr]
            ]
    timeBoxes :: E.AppEnv (_ (_ Event))
    timeBoxes = do
        V.fromList . fmap timeToBox <$> timeLabels

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
