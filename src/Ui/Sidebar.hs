{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Ui.Sidebar
    ( sidebarView
    ) where

import qualified GI.Gtk as Gtk
  ( ComboBoxText(..)
  , comboBoxSetActive
  , comboBoxTextAppendText
  , toComboBox
  )
import qualified GI.Gtk.Declarative as GD (BoxChild, afterCreated, widget)

import qualified Env as E (AppEnv)
import qualified Event as Ev (Event)

sidebarView :: E.AppEnv (GD.BoxChild Ev.Event)
sidebarView = pure $ GD.widget Gtk.ComboBoxText [GD.afterCreated addText]
  where
    addText :: _ => _widget -> IO ()
    addText widget = Gtk.comboBoxTextAppendText widget "Favorites"
        >> Gtk.comboBoxTextAppendText widget "Recents"
        >> Gtk.toComboBox widget >>= \box -> Gtk.comboBoxSetActive box 0
