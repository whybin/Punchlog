{-# LANGUAGE ExistentialQuantification #-}

module Event
    ( IsEvent(..)
    , Event(..)
    , Noop(..)
    ) where

import qualified GI.Gtk.Declarative.App.Simple as GD (Transition(..))

import qualified Env as E (Env)

data Event = forall e. IsEvent e => Event e

class IsEvent e where
  update :: E.Env -> e -> GD.Transition E.Env Event

data Noop = Noop

instance IsEvent Noop where
  update st Noop = GD.Transition st $ pure Nothing
