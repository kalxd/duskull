module Duskull.Scraper.Selector

import Duskull.FFI

%default total

export
data SelectorPtr : Type where

public export
record Selector where
    constructor MkSelector
    ptr: GCPtr SelectorPtr

%foreign (loadlib "selector_create")
prim__selectorCreate : String -> Ptr (Result AnyPtr)

%foreign (loadlib "selector_free")
prim__selectorFree : Ptr SelectorPtr -> PrimIO ()

%foreign (loadlib "selector_dbg")
prim__selectorDbg : GCPtr SelectorPtr -> PrimIO ()

dbg : Selector -> IO ()
dbg (MkSelector ptr) = primIO $ prim__selectorDbg ptr

free : Ptr SelectorPtr -> IO ()
free = primIO . prim__selectorFree

||| 创建选择器。支持css selectorg语法。
|||
||| ```idris example
||| mkSelector "body > .link .text"
||| ```
|||
export
mkSelector : HasIO io => String -> io (Either String Selector)
mkSelector css =
    case unpackResult $ prim__selectorCreate css of
        Right selector => (Right . MkSelector) <$> onCollect selector free
        Left e => pure $ Left e

main : IO ()
main =
    do s <- mkSelector "button.btn"
       case s of
           Right selector => dbg selector
           Left e => putStrLn e
