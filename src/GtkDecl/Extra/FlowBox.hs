{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module GtkDecl.Extra.FlowBox where

import Data.Vector (Vector)
import qualified GI.Gtk as Gtk

import GI.Gtk.Declarative.Bin (Bin)
import GI.Gtk.Declarative.Container.Class (IsContainer(..), ToChildren(..))

instance IsContainer Gtk.FlowBox (Bin Gtk.FlowBoxChild) where
    appendChild box _ widget' =
        Gtk.flowBoxInsert box widget' (-1)
    replaceChild box _ i old new = do
        Gtk.widgetDestroy old
        Gtk.flowBoxInsert box new i

instance ToChildren Gtk.FlowBox Vector (Bin Gtk.FlowBoxChild)
