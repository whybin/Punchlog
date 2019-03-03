module Ui.CreateTag
    ( CreateTagEvent(..)
    ) where

import qualified GI.Gtk.Declarative.App.Simple as GD (Transition(..))

import qualified Event as Ev (IsEvent(..))
import qualified TimeUnit as TU (TimeSlot)

newtype CreateTagEvent = CreateTag TU.TimeSlot

instance Ev.IsEvent CreateTagEvent where
  update st (CreateTag slot) = GD.Transition st $ pure Nothing
