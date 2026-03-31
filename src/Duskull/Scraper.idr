module Duskull.Scraper

import Control.Monad.Reader
import Control.Monad.Either
import Deriving.Show

import public Duskull.Scraper.Html
import public Duskull.Scraper.Element

%default total

export
record IOError where
    constructor MkIOError
    msg : String

Show IOError where
    show  e = "ScraperError: " ++ e.msg

public export
interface Nodeable a where
    covering
    select : (HasIO m, MonadError IOError m) => String -> a -> m (List Element)
    covering
    select1 : (HasIO m, MonadError IOError m) => String -> a -> m (Maybe Element)


Nodeable Html where
    select css doc = do
        Right xs <- liftIO $ Duskull.Scraper.Html.select css doc
              | Left e => throwError $ MkIOError e
        pure xs

    select1 css doc = do
        Right x <- liftIO $ Duskull.Scraper.Html.select1 css doc
              | Left e => throwError $ MkIOError e
        pure x

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
