module Duskull.Scraper.Html

import Duskull.FFI
import Duskull.Error
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

%foreign (loadlib "html_parse_doc")
prim__parseDocument : String -> Ptr HtmlPtr

%foreign (loadlib "html_parse_fragment")
prim__parseFragment : String -> Ptr HtmlPtr

%foreign (loadlib "html_select_free")
prim__htmlSelectFree : Ptr SelectPtr -> PrimIO ()

%foreign (loadlib "html_select")
prim__htmlSelect : GCPtr SelectorPtr -> GCPtr HtmlPtr -> Ptr SelectPtr

%foreign (loadlib "html_select_next")
prim__htmlSelectNext: Ptr SelectPtr -> Ptr ElementPtr

free : HasIO io => Ptr HtmlPtr -> io ()
free = primIO . prim__free

export
mkDocument : String -> Html
mkDocument css = let ptr = prim__parseDocument css
                 in MkHtml $ unsafePerformIO $ onCollect ptr free

export
mkFragment : String -> Html
mkFragment css = let ptr = prim__parseFragment css
                 in MkHtml $ unsafePerformIO $ onCollect ptr free

freeSelect : HasIO io => Ptr SelectPtr -> io ()
freeSelect = primIO . prim__htmlSelectFree

covering
reduceSelectToList : List Element -> Ptr SelectPtr -> List Element
reduceSelectToList acc ptr =
    let item = prim__htmlSelectNext ptr
    in if prim__nullPtr item == 1
          then acc
          else let el = castToElement item
               in reduceSelectToList (acc ++ [el]) ptr

covering
export
select : String -> Html -> List Element
select css (MkHtml htmlPtr) =
    let Right (MkSelector selectorPtr) = mkSelector css
        | Left e => []
        select = prim__htmlSelect selectorPtr htmlPtr
        xs = reduceSelectToList [] select
    in unsafePerformIO $ freeSelect select $> xs

covering
export
select1 : String -> Html -> Maybe Element
select1 css (MkHtml htmlPtr) =
    let Right (MkSelector selectorPtr) = mkSelector css
        | Left e => Nothing
        select = prim__htmlSelect selectorPtr htmlPtr
        item = prim__htmlSelectNext select
    in if prim__nullPtr item == 1
          then unsafePerformIO $ freeSelect select $> Nothing
          else let el = castToElement item
               in unsafePerformIO $ do
                   freeSelect select
                   pure $ Just el

%foreign loadlib "html_show"
prim__htmlShow : GCPtr HtmlPtr -> String

export
Show Html where
    show (MkHtml ptr) = prim__htmlShow ptr

covering
main : IO ()
main = do
    let doc = mkFragment """
    <button id="yes" go=1>button</button>
    """
    let Just el = select1 "#yes" doc
        | Nothing => pure ()
    printLn el
    printLn $ elementText el
    printLn $ elementAttr "go" el
    printLn $ elementAttr "unknown" el
