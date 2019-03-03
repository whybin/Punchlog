module Ui.TimeSlot
    ( onActivateTimeSlot
    ) where

import qualified GI.Gtk as Gtk (ListBox, ListBoxRow)

import qualified Env as E (AppEnv)
import qualified Event as Ev (IsEvent(..))
import qualified TimeUnit as TU (TimeUnit(..))
import qualified Ui.CreateTag as CT (CreateTagEvent(..))

onActivateTimeSlot
    -- :: Ev.IsEvent e => E.AppEnv (Maybe Gtk.ListBoxRow -> Gtk.ListBox -> IO e)
    :: E.AppEnv (Maybe Gtk.ListBoxRow -> Gtk.ListBox -> IO CT.CreateTagEvent)
onActivateTimeSlot = pure
    $ \row box -> putStrLn "clicked!" >> (pure $ CT.CreateTag (1, TU.Hour))
