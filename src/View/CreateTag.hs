{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}

module View.CreateTag
    ( CreateTagEvent(..)
    , createTagView
    ) where

import qualified Data.GI.Base.ManagedPtr as GI (unsafeCastTo)
import qualified Data.Text as T (Text)
import qualified GI.Gtk as Gtk
import GI.Gtk.Declarative (Attribute((:=)))
import qualified GI.Gtk.Declarative as GD
  ( Widget(..)
  , afterCreated
  , container
  , onM
  , widget
  )
import qualified GI.Gtk.Declarative.App.Simple as GD (Transition(..))
import qualified Lens.Micro.Platform as LM (set)

import qualified Env as E (AppEnv, creatingTag, state)
import qualified Event as Ev (Event(..), IsEvent(..))
import qualified TimeUnit as TU (TimeSlot)

data CreateTagEvent = CreateTag TU.TimeSlot
                    | NewTag T.Text

instance Ev.IsEvent CreateTagEvent where
  update st (CreateTag slot) =
      let st' = LM.set (E.state . E.creatingTag) (Just slot) st
       in GD.Transition st' $ pure Nothing
  update st (NewTag tagName) = GD.Transition st $ pure Nothing

createTagView :: E.AppEnv (GD.Widget Ev.Event)
createTagView = pure
    $ GD.container Gtk.Box [#orientation := Gtk.OrientationVertical]
        [ GD.widget Gtk.Entry [#placeholderText := "tag name"]
        , GD.widget Gtk.Button [#label := "Add tag", GD.onM #clicked onAddTag]
        ]
  where
    -- Unsafe event handler for clicking the 'Add tag' button
    onAddTag :: Gtk.Button -> IO Ev.Event
    onAddTag button = Gtk.getWidgetParent button >>=
        \case
            Nothing -> fail "No parent found for tag creation form button"
            Just container -> Gtk.containerGetChildren container >>=
                (\case
                    [ tagEntry, _ ] -> GI.unsafeCastTo Gtk.Entry tagEntry
                        >>= Gtk.entryGetText
                        >>= pure . Ev.Event . NewTag
                    _ -> fail "Failed to pattern match tag creation form box")
