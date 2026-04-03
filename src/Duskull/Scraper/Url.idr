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

containHost : String -> Url -> Bool
containHost host (MkUrl ptr) = prim__urlContainHost host ptr == 1

asFilePath : Url -> Maybe String
asFilePath (MkUrl ptr) =
    let p = prim__urlFilePath ptr
    in castMaybeString p

main : IO ()
main = do
    Right url <- newUrl "http://badi.com/hello/my/world"
          | Left e => putStrLn e
    dbg url
    printLn $ containHost "badi.com" url
    printLn $ asFilePath url
