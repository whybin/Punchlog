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

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (withReaderT)
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
import Lens.Micro.Platform ((%~))
import qualified Lens.Micro.Platform as LM (set, view)

import qualified Config as C (timeConfig)
import qualified Env as E (AppEnv, config, creatingTag, state, userData)
import qualified Event as Ev (Event(..), IsEvent(..))
import qualified Tag.Base as Tag (addTag, newTag)
import qualified Tag.Class as Tag (RawTag)
import qualified TimeUnit as TU (TimeSlot, TimestampUtc, timeSlotToTimestamp)
import qualified Util as U (untransformReader)
import qualified UserData as UD (tags)

data CreateTagEvent = CreateTag TU.TimeSlot
                    | NewTag Tag.RawTag

instance Ev.IsEvent CreateTagEvent where
  update st (CreateTag slot) =
      let st' = LM.set (E.state . E.creatingTag) (Just slot) st
       in GD.Transition st' $ pure Nothing
  update st (NewTag tag) =
      let st' = LM.set (E.state . E.creatingTag) Nothing st
          st'' = ((E.userData . UD.tags) %~ Tag.addTag tag) st'
       in GD.Transition st'' $ pure Nothing

createTagView :: TU.TimeSlot -> E.AppEnv (GD.Widget Ev.Event)
createTagView slot = do
    getTimestamp <- U.untransformReader
        $ withReaderT (LM.view $ E.config . C.timeConfig)
            (TU.timeSlotToTimestamp slot)
    pure $ GD.container Gtk.Box [#orientation := Gtk.OrientationVertical]
        [ GD.widget Gtk.Entry [#placeholderText := "tag name"]
        , GD.widget Gtk.Button
            [#label := "Add tag", GD.onM #clicked (onAddTag getTimestamp)]
        ]
  where
    -- Unsafe event handler for clicking the 'Add tag' button
    onAddTag :: IO TU.TimestampUtc -> Gtk.Button -> IO Ev.Event
    onAddTag getTimestamp button = do
        timestamp <- getTimestamp
        tagName <- readTagEntry button
        pure . Ev.Event . NewTag $ Tag.newTag timestamp tagName
    readTagEntry :: Gtk.Button -> IO T.Text
    readTagEntry button = Gtk.getWidgetParent button >>=
        \case
            Nothing -> fail "No parent found for tag creation form button"
            Just container -> Gtk.containerGetChildren container >>=
                (\case
                    [ tagEntry, _ ] -> GI.unsafeCastTo Gtk.Entry tagEntry
                        >>= Gtk.entryGetText
                    _ -> fail "Failed to pattern match tag creation form box")
