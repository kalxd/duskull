module Duskull.Scraper.Html

import Duskull.FFI
import Duskull.Scraper.Selector
import Duskull.Scraper.Element

import System.File

%default total

data HtmlPtr : Type where

export
record Html where
    constructor MkHtml
    ptr: GCPtr HtmlPtr

data SelectPtr : Type where

%foreign (loadlib "html_free")
prim__free : Ptr HtmlPtr -> PrimIO ()

%foreign (loadlib "html_parse_doc")
prim__parseDocument : String -> Ptr HtmlPtr

%foreign (loadlib "html_parse_fragment")
prim__parseFragment : String -> Ptr HtmlPtr

%foreign (loadlib "html_select_free")
prim__htmlSelectFree : Ptr SelectPtr -> PrimIO ()

%foreign (loadlib "html_select")
prim__htmlSelect: GCPtr HtmlPtr -> GCPtr SelectorPtr -> Ptr SelectPtr

%foreign (loadlib "html_select_next")
prim__htmlSelectNext: Ptr SelectPtr -> Ptr ElementPtr

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

freeSelect : Ptr SelectPtr -> IO ()
freeSelect = primIO . prim__htmlSelectFree

covering
reduceSelectToList : List Element -> Ptr SelectPtr -> IO (List Element)
reduceSelectToList acc ptr = do
    let item = prim__htmlSelectNext ptr
    case prim__nullPtr item of
        1 => pure acc
        _ => do
            el <- castToElement item
            reduceSelectToList (acc ++ [el]) ptr

covering
export
select : Html -> String -> IO (Either String (List Element))
select (MkHtml htmlPtr) css = do
    selector <- mkSelector css
    case selector of
        Left e => pure $ Left e
        Right (MkSelector selectorPtr) => do
            let select = prim__htmlSelect htmlPtr selectorPtr
            xs <- reduceSelectToList [] select
            freeSelect select
            pure $ Right xs

covering
main : IO ()
main = do
    content <- readFile "index.html"
    case content of
        Right content => do
            doc <- mkDocument content
            el <- select doc "header"
            putStrLn $ show $ length <$> el
        Left e => putStrLn $ show e
