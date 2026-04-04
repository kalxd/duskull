module Duskull.Scraper.Url

import Duskull.FFI

%default total

data UrlPtr : Type where

public export
record Url where
    constructor MkUrl
    ptr : GCPtr UrlPtr

%foreign (loadlib "url_show")
prim__urlShow : GCPtr UrlPtr -> String

%foreign (loadlib "url_free")
prim__urlFree : Ptr UrlPtr -> PrimIO ()

%foreign (loadlib "url_parse")
prim__urlParse : String -> Ptr (Result AnyPtr)

%foreign loadlib "url_unsafe_parse"
prim__urlUnsafeParse : String -> Ptr UrlPtr

%foreign loadlib "url_set_path"
prim__urlSetPath : String -> GCPtr UrlPtr -> Ptr UrlPtr

%foreign loadlib "url_has_same_host"
prim__urlHasSameHost : GCPtr UrlPtr -> GCPtr UrlPtr -> Bits8

%foreign loadlib "url_file_path"
prim__urlFilePath : GCPtr UrlPtr -> Ptr String

free : Ptr UrlPtr -> IO ()
free = primIO . prim__urlFree

show : Url -> String
show (MkUrl url) = prim__urlShow url

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

export
hasSameHost : Url -> Url -> Bool
hasSameHost (MkUrl targetPtr) (MkUrl sourcePtr) =
    prim__urlHasSameHost targetPtr sourcePtr == 1

export
asFilePath : Url -> Maybe String
asFilePath (MkUrl ptr) =
    let p = prim__urlFilePath ptr
    in castMaybeString p

namespace Unsafe
    export
    newUrl : HasIO io => String -> io Url
    newUrl url = let x = prim__urlUnsafeParse url
                 in MkUrl <$> onCollect x free

Show Url where
    showPrec d url = showCon d "URL" $ showArg (Url.show url)

main : IO ()
main = do
    url <- newUrl "http://baidu.com/your/are/sb"
    printLn url
    nextUrl <- setPath "not/sb" url
    printLn url
    printLn nextUrl

