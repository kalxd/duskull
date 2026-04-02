module Duskull.Request.Client

import Duskull.FFI
import Duskull.Error

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

%foreign (loadlib "client_make_request")
prim__clientMakeRequest : String -> Ptr (Result AnyPtr)

%foreign (loadlib "client_text")
prim__clientText : Ptr ClientPtr -> PrimIO (Ptr (Result AnyPtr))

dbg : HasIO io => Client -> io ()
dbg (MkClient ptr) = primIO $ prim__clientDbg ptr

free : HasIO io => Ptr ClientPtr -> io ()
free = primIO . prim__clientFree

makeRequest : String -> Either String Client
makeRequest url = MkClient <$> (unpackResult $ prim__clientMakeRequest url)

export
get : HasIO io
      => String
      -> (_ : (1 _: Client) -> io (Either String String))
      -> io (Either String String)
get url f =
    let Right client = makeRequest url
        | Left e => pure $ Left e
    in f client

export
text : HasIO io => (1 _ : Client) -> io (Either String String)
text (MkClient ptr) = do
    rsp <- primIO $ prim__clientText ptr
    case unpackResult rsp of
        Left e => pure $ Left e
        Right t => pure $ Right $ castToString t
