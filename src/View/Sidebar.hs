{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module View.Sidebar
    ( sidebarView
    ) where

import Control.Monad (when)

import qualified Data.Vector as V (Vector(..))
import qualified GI.Gdk as Gdk --(setRectangleWidth)
import qualified GI.Gtk.Objects.ComboBox as Gtk
import qualified GI.Gtk.Objects.ComboBoxText as Gtk
-- import qualified GI.Gtk as Gtk (Box(..), Orientation(..))
import qualified GI.Gtk as Gtk --(Box(..), Container, Orientation(..))
import GI.Gtk.Declarative (Attribute((:=)))
import qualified GI.Gtk.Declarative as GD
  ( BoxChild
  , Widget
  , afterCreated
  , classes
  , container
  , onM
  , widget
  )

import qualified Env as E (AppEnv)
import qualified Event as Ev (Event(..), Noop(..))

sidebarView :: E.AppEnv (GD.Widget Ev.Event)
sidebarView = boxUp <$> sequenceA [categorySortView]
  where
    boxUp :: V.Vector (GD.BoxChild Ev.Event) -> GD.Widget Ev.Event
    boxUp = GD.container Gtk.Box [ GD.classes ["sidebar"]
                                 , #orientation := Gtk.OrientationVertical
                                 ]

categorySortView :: E.AppEnv (GD.BoxChild Ev.Event)
categorySortView = pure $ GD.widget Gtk.ComboBoxText
    [ GD.afterCreated addText
    , GD.onM #sizeAllocate resize
    ]
  where
    addText :: _ => _widget -> IO ()
    addText widget = Gtk.comboBoxTextAppendText widget "Favorites"
        >> Gtk.comboBoxTextAppendText widget "Recents"
        >> Gtk.toComboBox widget >>= \box -> Gtk.comboBoxSetActive box 0
    resize :: Gdk.Rectangle -> Gtk.ComboBoxText -> IO Ev.Event
    resize rect box = do
        width <- Gdk.getRectangleWidth rect
        when (width > 50) $ Gdk.setRectangleWidth rect 50
        Gtk.widgetSizeAllocate box rect
        pure $ Ev.Event Ev.Noop
