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

%foreign (loadlib "html_dbg")
prim__dbg : GCPtr HtmlPtr -> PrimIO ()

free : Ptr HtmlPtr -> IO ()
free = primIO . prim__free

export
mkDocument : HasIO io => String -> io Html
mkDocument css = let ptr = prim__parseDocument css
                 in MkHtml <$> onCollect ptr free

export
mkFragment : HasIO io => String -> io Html
mkFragment css = let ptr = prim__parseFragment css
                 in MkHtml <$> onCollect ptr free

dbg : Html -> IO ()
dbg (MkHtml ptr) = primIO $ prim__dbg ptr

freeSelect : HasIO io => Ptr SelectPtr -> io ()
freeSelect = primIO . prim__htmlSelectFree

covering
reduceSelectToList : HasIO io => List Element -> Ptr SelectPtr -> io (List Element)
reduceSelectToList acc ptr = do
    let item = prim__htmlSelectNext ptr
    case prim__nullPtr item of
        1 => pure acc
        _ => do
            el <- castToElement item
            reduceSelectToList (acc ++ [el]) ptr

covering
export
select : HasIO io => String -> Html -> io (Either SomeError (List Element))
select css (MkHtml htmlPtr) = do
    selector <- mkSelector css
    case selector of
        Left e => pure $ Left e
        Right (MkSelector selectorPtr) => do
            let select = prim__htmlSelect selectorPtr htmlPtr
            xs <- reduceSelectToList [] select
            freeSelect select
            pure $ Right xs

covering
export
select1 : HasIO io => String -> Html -> io (Either SomeError (Maybe Element))
select1 css (MkHtml htmlPtr) = do
    selector <- mkSelector css
    case selector of
        Left e => pure $ Left e
        Right (MkSelector selectorPtr) => do
            let select = prim__htmlSelect selectorPtr htmlPtr
                item = prim__htmlSelectNext select
            if prim__nullPtr item == 1
                then pure $ Right Nothing
                else Right . Just <$> castToElement item

covering
main : IO ()
main = do
    doc <- mkFragment """
    <button id="yes" go=1>button</button>
    """
    el <- select1 "#yes" doc
    case el of
        Left e => putStrLn $ show e
        Right el => do
            printLn $ elementText =<< el
            printLn $ elementAttr "go" =<< el
            printLn $ elementAttr "unknown" =<< el
