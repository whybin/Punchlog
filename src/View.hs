module View where

import qualified GI.Gtk.Declarative as GD (Widget)

import qualified Event as Ev (Event)

type View = GD.Widget Ev.Event
