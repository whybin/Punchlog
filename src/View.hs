module View 
    ( View(..)
    ) where

import qualified GI.Gtk.Declarative as GD (Widget)

import qualified Event as Ev (Event)

type Widget = GD.Widget Ev.Event

data View = HPane Widget Widget
          | SinglePane Widget
