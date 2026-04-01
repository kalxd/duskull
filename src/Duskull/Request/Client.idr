module Duskull.Request.Client

import Duskull.FFI

%default total

data ClientPtr : Type where

export
record Client where
    constructor MkClient
    ptr : Ptr ClientPtr

%foreign (loadlib "client_dbg")
prim__clientDbg : Ptr ClientPtr -> PrimIO ()

%foreign (loadlib "client_free")
prim__clientFree : Ptr ClientPtr -> PrimIO ()

%foreign (loadlib "client_get")
prim__clientNew : String -> Ptr (Result AnyPtr)

%foreign (loadlib "client_text")
prim__clientText : Ptr ClientPtr -> PrimIO (Result String)

dbg : HasIO io => Client -> io ()
dbg (MkClient ptr) = primIO $ prim__clientDbg ptr

free : HasIO io => Ptr ClientPtr -> io ()
free = primIO . prim__clientFree

get : String -> Either String Client
get url = MkClient <$> (unpackResult $ prim__clientNew url)

main : IO ()
main = do
   let Right client = get "http://bin.com"
       | Left e => putStrLn e
   dbg client
