module Duskull.Scraper.Html

import Duskull.FFI
import Duskull.Scraper.Selector
import Duskull.Scraper.Element

%default total

data HtmlPtr : Type where

export
record Html where
    constructor MkHtml
    ptr: GCPtr HtmlPtr

data SelectPtr : Type where

%foreign (loadlib "html_free")
prim__free : Ptr HtmlPtr -> PrimIO ()

%foreign (loadlib "html_parse_document")
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

partial
reduceSelectToList : List Element -> Ptr SelectPtr -> IO (List Element)
reduceSelectToList acc ptr =
    let item = prim__htmlSelectNext ptr
    in if prim__nullPtr item == 0
       then pure acc
       else do
           el <- castToElement item
           reduceSelectToList (acc ++ [el]) ptr

partial
export
select : Html -> Selector -> IO (List Element)
select (MkHtml htmlPtr) (MkSelector selectorPtr) =
    let select = prim__htmlSelect htmlPtr selectorPtr
    in do xs <- reduceSelectToList [] select
          freeSelect select
          pure xs

main : IO ()
main = mkFragment "<button>yes</button>" >>= dbg
