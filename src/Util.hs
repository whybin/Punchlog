module Util
    ( transformReader
    , untransformReader
    ) where

import Control.Monad.Reader (Reader, ReaderT, mapReaderT)
import Data.Functor.Identity (Identity(..), runIdentity)

transformReader :: Monad m => Reader r a -> ReaderT r m a
transformReader = mapReaderT $ pure . runIdentity

untransformReader :: Monad m => ReaderT r m a -> Reader r (m a)
untransformReader = mapReaderT Identity
