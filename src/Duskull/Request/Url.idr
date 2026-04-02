module Duskull.Request.Url

import Duskull.FFI

%default total

data UrlPtr : Type where

public export
record Url where
    constructor MkUrl
    ptr : GCPtr UrlPtr

%foreign (loadlib "url_free")
prim__urlFree : Ptr UrlPtr -> PrimIO ()

%foreign (loadlib "url_dbg")
prim__urlDbg : GCPtr UrlPtr -> PrimIO ()

%foreign (loadlib "url_parse")
prim__urlParse : String -> Ptr (Result AnyPtr)

free : Ptr UrlPtr -> IO ()
free = primIO . prim__urlFree

dbg : Url -> IO ()
dbg (MkUrl url) = primIO $ prim__urlDbg url

export
newUrl : HasIO io => String -> io (Either String Url)
newUrl input = do
    let url = prim__urlParse input
    case unpackResult url of
        Left e => pure $ Left e
        Right url => Right . MkUrl <$> onCollect url free

main : IO ()
main = do
    Right url <- newUrl "http://badi.com"
          | Left e => putStrLn e
    dbg url
