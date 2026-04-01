module Duskull.Request.Client

import Duskull.FFI

%default total

data ClientPtr : Type where

export
record Client where
    constructor MkClient
    ptr : GCPtr ClientPtr

%foreign (loadlib "client_dbg")
prim__clientDbg : GCPtr ClientPtr -> PrimIO ()

%foreign (loadlib "client_free")
prim__clientFree : Ptr ClientPtr -> PrimIO ()

%foreign (loadlib "client_get")
prim__clientNew : String -> Ptr (Result AnyPtr)

dbg : HasIO io => Client -> io ()
dbg (MkClient ptr) = primIO $ prim__clientDbg ptr

free : HasIO io => Ptr ClientPtr -> io ()
free = primIO . prim__clientFree

get : HasIO io => String -> io (Either String Client)
get url = do
    case unpackResult $ prim__clientNew url of
        Left e => pure $ Left e
        Right client => Right . MkClient <$> onCollect client free
