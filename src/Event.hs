{-# LANGUAGE ExistentialQuantification #-}

module Event
    ( IsEvent(..)
    , Event(..)
    , BasicEvent(..)
    ) where

import qualified GI.Gtk.Declarative.App.Simple as GD (Transition(..))

import qualified Env as E (Env)

data Event = forall e. IsEvent e => Event e

class IsEvent e where
  update :: E.Env -> e -> GD.Transition E.Env Event

data BasicEvent = Quit
                | Noop

instance IsEvent BasicEvent where
  update st Quit = GD.Exit
  update st Noop = GD.Transition st $ pure Nothing
