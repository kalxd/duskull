module Duskull.Scraper.Selector

import Duskull.FFI
import Duskull.Error

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

free : HasIO io => Ptr SelectorPtr -> io ()
free = primIO . prim__selectorFree

||| 创建选择器。支持css selectorg语法。
|||
||| ```idris example
||| mkSelector "body > .link .text"
||| ```
|||
export
mkSelector : String -> Either SomeError Selector
mkSelector css =
    let Right selector = unpackResult $ prim__selectorCreate css
        | Left e => Left $ parseError e
    in Right . MkSelector $ unsafePerformIO $ onCollect selector free

main : IO ()
main = do
    let Right s = mkSelector "button.btn"
        | Left e => printLn e
    dbg s
