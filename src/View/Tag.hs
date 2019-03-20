{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module View.Tag
    ( tagView
    ) where

import qualified GI.Gtk as Gtk (Label(..))
import GI.Gtk.Declarative (Attribute((:=)))
import qualified GI.Gtk.Declarative as GD (widget)

import qualified Env as E (AppEnv)
import qualified Tag.Class as Tag (RawTag(..))
import qualified View.Type as V (View)

tagView :: Tag.RawTag -> E.AppEnv V.View
tagView tag = pure $
    GD.widget Gtk.Label [#label := Tag.text tag]
