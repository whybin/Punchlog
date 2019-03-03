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
import Control.Monad.Reader (ask, runReader, withReader)

import qualified Data.Text as T (pack)
import qualified Data.Vector as V (fromList)
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative (Attribute((:=)))
import qualified GI.Gtk.Declarative as GD
import qualified GI.Gtk.Declarative.App.Simple as GD

import qualified Env as E
import qualified Event as Ev (BasicEvent(..), Event(..), IsEvent(..))
import qualified GtkDecl.Extra.FlowBox
import qualified Config as C (Config(..))
import qualified State as S (State(..))
import qualified TimeUnit as TU (minuteMarks)
import qualified Ui.Sidebar as Si (sidebarView)
import qualified Ui.TimeSlot as TS (onActivateTimeSlot)
import qualified UserData as UD (UserData)
import qualified View as V

type AppState = E.Env

update :: E.Env -> Ev.Event -> GD.Transition E.Env Ev.Event
update st (Ev.Event ev) = Ev.update st ev

view :: AppState -> GD.AppView Gtk.Window Ev.Event
view st = GD.bin Gtk.Window [ #title := "Punchlog"
                            , #defaultWidth := 600
                            , #defaultHeight := 640
                            , GD.on #deleteEvent
                                (const (True, Ev.Event Ev.Quit))
                            ]
            $ (runReader . E.runAppEnv $ readView) st

readView :: E.AppEnv (GD.Widget Ev.Event)
readView = do
    st <- E.state <$> E.liftToAppEnv ask
    case S.view st of
      Just v -> processView v
      Nothing -> mainView >>= processView
  where
    processView :: V.View -> E.AppEnv (GD.Widget Ev.Event)
    processView view' =
        case view' of
          V.HPane lview rview ->
              pure $ GD.paned [] (GD.pane GD.defaultPaneProperties lview)
                                 (GD.pane GD.defaultPaneProperties rview)
          V.SinglePane sview -> pure sview

mainView :: E.AppEnv V.View
mainView = V.HPane <$> Si.sidebarView <*> dayView

dayView :: E.AppEnv (GD.Widget Ev.Event)
dayView = do
    hoursView' <- hoursView
    pure $ GD.bin Gtk.Viewport [] $
        GD.container Gtk.Box [ #orientation := Gtk.OrientationVertical ]
            [ GD.widget Gtk.Label [ #label := "Today", #xalign := 0 ]
            , hoursView'
            ]
  where
    hoursView :: E.AppEnv (_ Ev.Event)
    hoursView = do
        boxes <- timeBoxes
        onActivate <- TS.onActivateTimeSlot
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
    timeToBox :: String -> _ Ev.Event
    timeToBox timeStr =
        GD.bin Gtk.ListBoxRow []
        $ GD.container Gtk.FlowBox [#selectionMode := Gtk.SelectionModeNone]
            [ GD.bin Gtk.FlowBoxChild []
                $ GD.widget Gtk.Label [#label := T.pack timeStr]
            ]
    timeBoxes :: E.AppEnv (_ (_ Ev.Event))
    timeBoxes = do
        V.fromList . fmap timeToBox <$> timeLabels

initialState :: C.Config -> UD.UserData -> AppState
initialState config userData =
    E.Env { E.config = config
          , E.state = S.State { S.view = Nothing
                              }
          , E.userData = userData
          }

runApp :: C.Config -> UD.UserData -> IO ()
runApp config userData = void $
    GD.run GD.App { GD.update = update
                  , GD.view = view
                  , GD.inputs = []
                  , GD.initialState = initialState config userData
                  }
