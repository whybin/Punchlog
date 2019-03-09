{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module View.Schedule
    ( scheduleView
    ) where

import Control.Applicative (liftA2)
import Control.Monad.Reader (withReader)

import qualified Data.Text as T (pack)
import qualified Data.Vector as V (fromList)
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative (Attribute((:=)))
import qualified GI.Gtk.Declarative as GD
import qualified Lens.Micro.Platform as LM (view)

import qualified Config as C (timeConfig)
import qualified Env as E (AppEnv, config)
import qualified Event as Ev (Event)
import qualified GtkDecl.Extra.FlowBox
import qualified TimeUnit as TU (minuteMarks)
import qualified Ui.TimeSlot as TS (onActivateTimeSlot)
import qualified View.Type as V (View)
import qualified View.Sidebar as VS (sidebarView)

scheduleView :: E.AppEnv V.View
scheduleView = do
    lview <- VS.sidebarView
    rview <- dayView
    pure $ GD.paned [] (GD.pane GD.defaultPaneProperties lview)
                       (GD.pane GD.defaultPaneProperties rview)

dayView :: E.AppEnv V.View
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
    timeLabels = (liftA2 (++) (map show [0..23]) . map (':':))
        <$> withReader (LM.view $ E.config . C.timeConfig) TU.minuteMarks
    timeToBox :: String -> _ Ev.Event
    timeToBox timeStr =
        GD.bin Gtk.ListBoxRow []
        $ GD.container Gtk.FlowBox [#selectionMode := Gtk.SelectionModeNone]
            [ GD.bin Gtk.FlowBoxChild []
                $ GD.widget Gtk.Label [#label := T.pack timeStr]
            ]
    timeBoxes :: E.AppEnv (_ (_ Ev.Event))
    timeBoxes = V.fromList . fmap timeToBox <$> timeLabels
