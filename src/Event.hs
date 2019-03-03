{-# LANGUAGE TypeFamilies #-}

module Event
    ( IsEvent(..)
    , Event(..)
    ) where

import qualified GI.Gtk.Declarative.App.Simple as GD (Transition(..))

import qualified Env as E (Env)

class IsEvent e where
  update :: E.Env -> e -> GD.Transition E.Env e

data Event = Quit
           | Noop
           | CreateTag

instance IsEvent Event where
  update st Quit = GD.Exit
  update st Noop = GD.Transition st $ pure Nothing
  update st CreateTag = GD.Transition st $ pure Nothing
