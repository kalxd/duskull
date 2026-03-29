module Duskull.Scraper.Element

import Duskull.FFI

export
data ElementPtr : Type where

public export
record Element where
    constructor MkElement
    ptr: GCPtr ElementPtr

%foreign (loadlib "to_str")
prim__toStr: Ptr String -> String

%foreign (loadlib "element_free")
prim__elementFree: Ptr ElementPtr -> PrimIO ()

%foreign (loadlib "element_id")
prim__elementId: GCPtr ElementPtr -> Ptr String

%foreign (loadlib "element_attr")
prim__elementAttr: String -> GCPtr ElementPtr -> Ptr String

export
free: Ptr ElementPtr -> IO ()
free = primIO . prim__elementFree

export
castToElement: Ptr ElementPtr -> IO Element
castToElement ptr = MkElement <$> onCollect ptr free

export
elementId: Element -> Maybe String
elementId (MkElement ptr) =
    let id = prim__elementId ptr
    in if prim__nullPtr id == 1
       then Nothing
       else Just $ prim__toStr id

export
elementAttr: String -> Element -> Maybe String
elementAttr css (MkElement ptr) =
    let attr = prim__elementAttr css ptr
    in if prim__nullPtr attr == 1
       then Nothing
       else Just $ prim__toStr attr
