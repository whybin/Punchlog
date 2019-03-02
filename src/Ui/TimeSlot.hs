module Ui.TimeSlot
    ( onActivateTimeSlot
    ) where

import qualified GI.Gtk as Gtk (ListBox, ListBoxRow)

import qualified Env as E (AppEnv)
import qualified Event as Ev (Event(..))

onActivateTimeSlot
    :: E.AppEnv (Maybe Gtk.ListBoxRow -> Gtk.ListBox -> IO Ev.Event)
onActivateTimeSlot = pure
    $ \row box -> putStrLn "clicked!" >> pure Ev.Noop
