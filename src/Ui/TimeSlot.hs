module Ui.TimeSlot
    ( onActivateTimeSlot
    ) where

import qualified GI.Gtk as Gtk (ListBox, ListBoxRow)

import qualified Env as E (AppEnv)
import qualified Event as Ev (Event(..))
import qualified TimeUnit as TU (TimeUnit(..))
import qualified Ui.CreateTag as CT (CreateTagEvent(..))

onActivateTimeSlot
    :: E.AppEnv (Maybe Gtk.ListBoxRow -> Gtk.ListBox -> IO Ev.Event)
onActivateTimeSlot = pure
    $ \row box -> putStrLn "clicked!"
    >> (pure $ Ev.Event $ CT.CreateTag (1, TU.Hour))
