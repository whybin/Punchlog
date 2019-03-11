module Util
    ( liftReader
    , untransformReader
    ) where

import Control.Monad.Reader (Reader, ReaderT(..), mapReaderT, runReader)
import Data.Functor.Identity (Identity(..))

liftReader :: Monad m => Reader r a -> ReaderT r m a
liftReader reader = ReaderT $ return . runReader reader

untransformReader :: Monad m => ReaderT r m a -> Reader r (m a)
untransformReader = mapReaderT Identity
