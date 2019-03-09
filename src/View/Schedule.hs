{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE LambdaCase #-}
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
import qualified Env as E (AppEnv, config, creatingTag, state)
import qualified Event as Ev (Event)
import qualified GtkDecl.Extra.FlowBox
import qualified TimeUnit as TU (TimeSlot, timeSlots)
import qualified View.CreateTag as CT (createTagView)
import qualified View.TimeSlot as TS (onActivateTimeSlot)
import qualified View.Type as V (View)
import qualified View.Sidebar as VS (sidebarView)

scheduleView :: E.AppEnv V.View
scheduleView = do
    lview <- VS.sidebarView
    GD.paned [] (GD.pane GD.defaultPaneProperties lview)
        . GD.pane GD.defaultPaneProperties
        <$> dayView

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
    timeSlots :: E.AppEnv [TU.TimeSlot]
    timeSlots = withReader (LM.view $ E.config . C.timeConfig) TU.timeSlots
    wrapListBoxRow :: _ Ev.Event -> _ Ev.Event
    wrapListBoxRow = GD.bin Gtk.ListBoxRow []
    timeToBox :: TU.TimeSlot -> E.AppEnv (_ Ev.Event)
    timeToBox slot =
        let innerBox = GD.container Gtk.FlowBox
                [#selectionMode := Gtk.SelectionModeNone]
                [ GD.bin Gtk.FlowBoxChild []
                    $ GD.widget Gtk.Label [#label := T.pack (show slot)]
                ]
         in do
             createTagView <- CT.createTagView
             wrapListBoxRow . \case
                 Just slot' | slot == slot' -> GD.container Gtk.ListBox []
                    [ wrapListBoxRow innerBox
                    , wrapListBoxRow createTagView
                    ]
                 _ -> innerBox
                 <$> LM.view (E.state . E.creatingTag)
    timeBoxes :: E.AppEnv (_ (_ Ev.Event))
    timeBoxes = timeSlots >>= fmap V.fromList . traverse timeToBox
