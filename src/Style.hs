{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Style
    ( initDefaultStyle
    ) where

import qualified Data.ByteString as BS (ByteString)
import qualified GI.Gdk as Gdk (screenGetDefault)
import qualified GI.Gtk as Gtk
import Text.RawString.QQ (r)

defaultStyle :: BS.ByteString
defaultStyle = [r|
.sidebar {
    background-color: #29292d;
}
               |]

initDefaultStyle :: IO ()
initDefaultStyle = do
    screen <- Gdk.screenGetDefault >>= maybe (fail "No default screen") return
    cssProvider <- Gtk.cssProviderNew
    Gtk.cssProviderLoadFromData cssProvider defaultStyle
    Gtk.styleContextAddProviderForScreen screen cssProvider
        $ fromIntegral Gtk.STYLE_PROVIDER_PRIORITY_USER
