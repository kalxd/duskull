module Duskull.Control.Scraper

import Control.Monad.Reader
import Duskull.Scraper.Html
import Duskull.Scraper.Element
import Duskull.FFI

%default total

export
runDocument : HasIO m => String -> ReaderT Html m a -> m a
runDocument str x = do
    doc <- mkDocument str
    runReaderT doc x

export
runFragment : HasIO m => String -> ReaderT Html m a -> m a
runFragment str x = do
    doc <- mkFragment str
    runReaderT doc x
