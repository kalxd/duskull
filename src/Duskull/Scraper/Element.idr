module Duskull.Scraper.Element

import Duskull.FFI

export
data ElementPtr : Type where

public export
record Element where
    constructor MkElement
    ptr: GCPtr ElementPtr

%foreign (loadlib "element_free")
prim__elementFree: Ptr ElementPtr -> PrimIO ()

export
free: Ptr ElementPtr -> IO ()
free = primIO . prim__elementFree

export
castToElement: Ptr ElementPtr -> IO Element
castToElement ptr = MkElement <$> onCollect ptr free
