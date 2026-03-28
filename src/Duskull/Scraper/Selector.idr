module Duskull.Scraper.Selector

import Duskull.FFI

%default total

public export
data Selector : Type where

%foreign (loadlib "selector_create")
prim__selectorCreate : String -> Ptr (Result AnyPtr)

%foreign (loadlib "selector_free")
prim__selectorFree : Ptr Selector -> PrimIO ()

%foreign (loadlib "selector_dbg")
prim__selectorDbg : GCPtr Selector -> PrimIO ()

dbg : GCPtr Selector -> IO ()
dbg = primIO . prim__selectorDbg

free : Ptr Selector -> IO ()
free = primIO . prim__selectorFree

||| 创建选择器。支持css selectorg语法。
|||
||| ```idris example
||| mkSelector "body > .link .text"
||| ```
|||
export
mkSelector : String -> IO (Either String (GCPtr Selector))
mkSelector css =
    case unpackResult $ prim__selectorCreate css of
        Right selector => Right <$> onCollect selector free
        Left e => pure $ Left e

main : IO ()
main =
    do s <- mkSelector "button.btn"
       case s of
           Right selector => dbg selector
           Left e => putStrLn e
