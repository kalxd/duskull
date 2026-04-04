module Duskull.Scraper.Url

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

%foreign loadlib "url_unsafe_parse"
prim__urlUnsafeParse : String -> Ptr UrlPtr

%foreign loadlib "url_set_path"
prim__urlSetPath : String -> GCPtr UrlPtr -> Ptr UrlPtr

%foreign loadlib "url_contain_host"
prim__urlContainHost : String -> GCPtr UrlPtr -> Bits8

%foreign loadlib "url_file_path"
prim__urlFilePath : GCPtr UrlPtr -> Ptr String

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

export
setPath : HasIO io => String -> Url -> io Url
setPath path (MkUrl ptr) =
    let newurl = prim__urlSetPath path ptr
    in MkUrl <$> onCollect newurl free

containHost : String -> Url -> Bool
containHost host (MkUrl ptr) = prim__urlContainHost host ptr == 1

asFilePath : Url -> Maybe String
asFilePath (MkUrl ptr) =
    let p = prim__urlFilePath ptr
    in castMaybeString p

namespace Unsafe
    export
    newUrl : HasIO io => String -> io Url
    newUrl url = let x = prim__urlUnsafeParse url
                 in MkUrl <$> onCollect x free

main : IO ()
main = do
    url <- newUrl "http://baidu.com/your/are/sb"
    dbg url
    nextUrl <- setPath "not/sb" url
    dbg url
    dbg nextUrl

