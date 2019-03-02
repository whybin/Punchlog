module Ui.TimeChunk
    ( onActivateTimeChunk
    ) where

import qualified GI.Gtk as Gtk (ListBox, ListBoxRow)

import qualified Env as E (AppEnv)
import qualified Event as Ev (Event(..))

onActivateTimeChunk
    :: E.AppEnv (Maybe Gtk.ListBoxRow -> Gtk.ListBox -> IO Ev.Event)
onActivateTimeChunk = pure
    $ \row box -> putStrLn "clicked!" >> pure Ev.Noop
