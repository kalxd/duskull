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
prim__clientText : Ptr ClientPtr -> PrimIO (Ptr (Result AnyPtr))

dbg : HasIO io => Client -> io ()
dbg (MkClient ptr) = primIO $ prim__clientDbg ptr

free : HasIO io => Ptr ClientPtr -> io ()
free = primIO . prim__clientFree

get : String -> Either String Client
get url = MkClient <$> (unpackResult $ prim__clientNew url)

text : HasIO io => (1 _ : Client) -> io (Either String String)
text (MkClient ptr) = do
    rsp <- primIO $ prim__clientText ptr
    case unpackResult rsp of
        Left e => pure $ Left e
        Right t => pure $ Right $ castToString t

main : IO ()
main = do
   let Right client = get "https://httpbin.io/json"
       | Left e => putStrLn e
   rsp <- text client
   printLn rsp
