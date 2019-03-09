{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module View.CreateTag
    ( CreateTagEvent(..)
    , createTagView
    ) where

import qualified GI.Gtk as Gtk (Label(..))
import GI.Gtk.Declarative (Attribute((:=)))
import qualified GI.Gtk.Declarative as GD (Widget(..), widget)
import qualified GI.Gtk.Declarative.App.Simple as GD (Transition(..))
import qualified Lens.Micro.Platform as LM (set)

import qualified Env as E (AppEnv, creatingTag, state)
import qualified Event as Ev (Event, IsEvent(..))
import qualified TimeUnit as TU (TimeSlot)

newtype CreateTagEvent = CreateTag TU.TimeSlot

instance Ev.IsEvent CreateTagEvent where
  update st (CreateTag slot) =
      let st' = LM.set (E.state . E.creatingTag) (Just slot) st
       in GD.Transition st' $ pure Nothing

createTagView :: E.AppEnv (GD.Widget Ev.Event)
createTagView = pure $
    GD.widget Gtk.Label [#label := "create tag"]
