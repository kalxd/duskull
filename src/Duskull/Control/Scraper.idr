module Duskull.Control.Scraper

import Control.App
import Duskull.Scraper.Html
import Duskull.Scraper.Element
import Duskull.FFI

%default total

export
interface Scraper e where
    mkDocument : String -> App {l} e Html
    mkFragment : String -> App {l} e Html

PrimIO e => Scraper e where
    mkDocument = primIO . Duskull.Scraper.Html.mkDocument
    mkFragment = primIO . Duskull.Scraper.Html.mkFragment

covering
export
select : (PrimIO e, Scraper e, HasErr FFIError e)
         => String -> Html -> App e (List Element)
select css doc = do
    val <- primIO $ Duskull.Scraper.Html.select css doc
    case val of
        Left e => throw $ ParseError e
        Right xs => pure xs

covering
export
select1 : (PrimIO e, Scraper e, HasErr FFIError e)
          => String -> Html -> App e (Maybe Element)
select1 css doc = do
    val <- primIO $ Duskull.Scraper.Html.select1 css doc
    case val of
        Left e => throw $ ParseError e
        Right x => pure x
