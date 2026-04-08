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

%foreign loadlib "selector_show"
prim__selectorShow : GCPtr SelectorPtr -> String

show : Selector -> String
show (MkSelector ptr) = prim__selectorShow ptr

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
        | Left e => Left $ ParseError $ css ++ "不是有效的css。 =>" ++ e
    in Right . MkSelector $ unsafePerformIO $ onCollect selector free

export
Show Selector where
    show = Selector.show

main : IO ()
main = do
    let Right s = mkSelector "button.btn"
        | Left e => printLn e
    printLn s
