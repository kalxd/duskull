module Duskull.Scraper.Request

import Duskull.FFI
import Duskull.Error
import Duskull.Scraper.Url

%default total

data RequestPtr : Type where

export
record Request where
    constructor MkRequest
    ptr : Ptr RequestPtr

%foreign loadlib "request_free"
prim__requestFree : Ptr RequestPtr -> PrimIO ()

free : HasIO io => Ptr RequestPtr -> io ()
free = primIO . prim__requestFree

%foreign loadlib "request_make_get"
prim__requestMakeGet : GCPtr UrlPtr -> Ptr RequestPtr

makeGet : Url -> Request
makeGet (MkUrl ptr) = MkRequest $ prim__requestMakeGet ptr

%foreign loadlib "request_set_header"
prim__requestSetHeader : String -> String -> (1 _ : Ptr RequestPtr) -> Ptr RequestPtr

export
setHeader : String -> String -> (1 _ : Request) -> Request
setHeader key value (MkRequest ptr) =
    let req = prim__requestSetHeader key value ptr
    in MkRequest req

%foreign loadlib "request_text"
prim__requestText : (1 _ : Ptr RequestPtr) -> PrimIO (Ptr (Result AnyPtr))

export
text : HasIO io => (1 _ : Request) -> io (Either SomeError String)
text (MkRequest ptr) = do
    val <- primIO $ prim__requestText ptr
    case unpackResult val of
        Left e => pure $ Left $ ioError e
        Right val => pure $ Right $ castToString val

%foreign loadlib "request_download"
prim__requestDownload : String -> (1 _ : Ptr RequestPtr) -> PrimIO (Ptr String)

export
download : HasIO io => String -> (1 _ : Request) -> io (Either SomeError ())
download filepath (MkRequest ptr) = do
    val <- primIO $ prim__requestDownload filepath ptr
    case castMaybeString val of
        Just e => pure $ Left $ ioError e
        _ => pure $ Right ()

%foreign loadlib "request_show"
prim__requestShow : Ptr RequestPtr -> String

show : Request -> String
show (MkRequest ptr) = prim__requestShow ptr

export
get : HasIO io
      => Url
      -> (_ : (1 _: Request) -> io (Either SomeError a))
      -> io (Either SomeError a)
get url f = f $ makeGet url

Show Request where
    show = Request.show

main : IO ()
main = do
    let url = newUrl "http://httpbin.io/image/png"
    Right rsp <- get url $ download "sample.png"
    | Left e => printLn e
    pure ()

