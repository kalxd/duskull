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

covering
select : (HasIO m, MonadError IOError m, MonadReader Html m)
         => String -> m (List Element)
select css = do
    Right xs <- liftIO $ Duskull.Scraper.Html.select css !ask
          | Left e => throwError $ MkIOError e
    pure xs

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

export
attr : String -> Element -> Maybe String
attr = elementAttr

export
attr' : MonadError IOError m => String -> Element -> m String
attr' name el =
    case attr name el of
        Just a => pure a
        Nothing => throwError $ MkIOError $ "属性(" ++ name ++ ")不存在！"
