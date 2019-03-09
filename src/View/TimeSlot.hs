module View.TimeSlot
    ( onActivateTimeSlot
    ) where

import qualified GI.Gtk as Gtk (ListBox, ListBoxRow, listBoxRowGetIndex)
import qualified Lens.Micro.Platform as LM (view)

import qualified Config as C (timeConfig)
import qualified Env as E (AppEnv, config)
import qualified Event as Ev (Event(..), Noop(..))
import qualified TimeUnit as TU (TimeSlot(..), TimeUnit(..), unit)
import qualified View.CreateTag as CT (CreateTagEvent(..))

onActivateTimeSlot
    :: E.AppEnv (Maybe Gtk.ListBoxRow -> Gtk.ListBox -> IO Ev.Event)
onActivateTimeSlot = do
    unit <- LM.view $ E.config . C.timeConfig . TU.unit
    pure $ \row _ -> maybe (pure $ Ev.Event Ev.Noop) (eventForRow unit) row
  where
    eventForRow :: TU.TimeUnit -> Gtk.ListBoxRow -> IO Ev.Event
    eventForRow unit row = do
        rowIdx <- Gtk.listBoxRowGetIndex row
        pure . Ev.Event . CT.CreateTag
            $ TU.TimeSlot (fromIntegral rowIdx, unit)
