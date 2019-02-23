module Util where

import Control.Monad.Reader (Reader, ReaderT(..), runReader)

liftReader :: Monad m => Reader r a -> ReaderT r m a
liftReader reader = ReaderT $ return . runReader reader
