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

Control.App.PrimIO e => Scraper e where
    mkDocument = primIO . Duskull.Scraper.Html.mkDocument
    mkFragment = primIO . Duskull.Scraper.Html.mkFragment

covering
export
select : (Control.App.PrimIO e, Scraper e, HasErr FFIError e)
         => String -> Html -> App e (List Element)
select css doc = do
    val <- primIO $ Duskull.Scraper.Html.select css doc
    case val of
        Left e => throw $ ParseError e
        Right xs => pure xs
