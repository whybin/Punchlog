{-# LANGUAGE ExistentialQuantification #-}

module Event
    ( IsEvent(..)
    , Event(..)
    , BasicEvent(..)
    ) where

import qualified GI.Gtk.Declarative.App.Simple as GD (Transition(..))

data Event = forall e. IsEvent e => Event e

class IsEvent e where
  update :: st -> e -> GD.Transition st Event

data BasicEvent = Quit
                | Noop

instance IsEvent BasicEvent where
  update st Quit = GD.Exit
  update st Noop = GD.Transition st $ pure Nothing
