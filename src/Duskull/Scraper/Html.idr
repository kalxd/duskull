module Duskull.Scraper.Html

import Duskull.FFI

%default total

data HtmlPtr : Type where

export
record Html where
    constructor MkHtml
    ptr: GCPtr HtmlPtr

%foreign (loadlib "html_free")
prim__free : Ptr HtmlPtr -> PrimIO ()

%foreign (loadlib "html_parse_document")
prim__parseDocument : String -> Ptr HtmlPtr

%foreign (loadlib "html_parse_fragment")
prim__parseFragment : String -> Ptr HtmlPtr

%foreign (loadlib "html_dbg")
prim__dbg : GCPtr HtmlPtr -> PrimIO ()

free : Ptr HtmlPtr -> IO ()
free = primIO . prim__free

export
mkDocument : String -> IO Html
mkDocument css = let ptr = prim__parseDocument css
                 in MkHtml <$> onCollect ptr free

export
mkFragment : String -> IO Html
mkFragment css = let ptr = prim__parseFragment css
                 in MkHtml <$> onCollect ptr free

dbg : Html -> IO ()
dbg (MkHtml ptr) = primIO $ prim__dbg ptr

main : IO ()
main = mkFragment "<button>yes</button>" >>= dbg
