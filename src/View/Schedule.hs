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
import qualified Env as E (AppEnv, config, creatingTag, state, userData)
import qualified Event as Ev (Event)
import qualified GtkDecl.Extra.FlowBox
import qualified Tag.Base as Tag (tagsOnDay)
import qualified Tag.Class as Tag (RawTag(..))
import qualified TimeUnit as TU
  ( TimeSlot
  , TimestampUtc(..)
  , localToday
  , timeSlots
  )
import qualified UserData as UD (tags)
import qualified View.CreateTag as CT (createTagView)
import qualified View.Sidebar as VS (sidebarView)
import qualified View.State as V (Day, calendarDay)
import qualified View.Tag as VT (tagView)
import qualified View.TimeSlot as TS (onActivateTimeSlot)
import qualified View.Type as V (View)

scheduleView :: V.Day -> E.AppEnv V.View
scheduleView day = do
    lview <- VS.sidebarView
    GD.paned [] (GD.pane GD.defaultPaneProperties lview)
        . GD.pane GD.defaultPaneProperties
        <$> dayView day

dayView :: V.Day -> E.AppEnv V.View
dayView day = do
    hoursView' <- hoursView
    pure $ GD.bin Gtk.Viewport [] $
        GD.container Gtk.Box
            [ GD.classes ["day"]
            , #orientation := Gtk.OrientationVertical
            ]
            [ GD.widget Gtk.Label [#label := T.pack (show day), #xalign := 0]
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
    day'sTags :: E.AppEnv [Tag.RawTag]
    day'sTags = do
        allTags <- LM.view (E.userData . UD.tags)
        pure . Tag.tagsOnDay allTags $ V.calendarDay day
    timeToBox :: TU.TimeSlot -> E.AppEnv (_ Ev.Event)
    timeToBox slot = do
        tags <- filter ((== slot) . TU.timeSlotUtc . Tag.time) <$> day'sTags
        tagBoxes <- traverse tagToBox tags
        let innerBox = GD.container Gtk.FlowBox
                [#selectionMode := Gtk.SelectionModeNone]
                $ V.fromList (label : reverse tagBoxes)
            label = GD.bin Gtk.FlowBoxChild []
                $ GD.widget Gtk.Label [#label := T.pack (show slot)]
        createTagView <- CT.createTagView slot
        GD.bin Gtk.ListBoxRow [] . \case
            Just slot' | slot == slot' -> GD.container Gtk.Box
                [#orientation := Gtk.OrientationVertical]
                [ GD.BoxChild GD.defaultBoxChildProperties innerBox
                , GD.BoxChild GD.defaultBoxChildProperties createTagView
                ]
            _ -> innerBox
            <$> LM.view (E.state . E.creatingTag)
    timeBoxes :: E.AppEnv (_ (_ Ev.Event))
    timeBoxes = timeSlots >>= fmap V.fromList . traverse timeToBox

timeSlots :: E.AppEnv [TU.TimeSlot]
timeSlots = withReader (LM.view $ E.config . C.timeConfig) TU.timeSlots

tagToBox :: Tag.RawTag -> E.AppEnv (GD.Bin Gtk.FlowBoxChild Ev.Event)
tagToBox = fmap (GD.bin Gtk.FlowBoxChild []) . VT.tagView
