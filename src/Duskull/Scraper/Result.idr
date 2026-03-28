module Duskull.Scraper.Result

import Duskull.FFI

%default total

%foreign (loadlib "result_is_error")
prim__resultIsError : Ptr (Result a) -> Bits8

%foreign (loadlib "result_err_msg")
prim__resultErrorMsg : (1 _: Ptr (Result a)) -> String

%foreign (loadlib "result_value")
prim__resultValue : (1 _: Ptr (Result a)) -> a

export
unpackResult : Ptr (Result a) -> Either String a
unpackResult val =
    case prim__resultIsError val of
        1 => Left $ prim__resultErrorMsg val
        _ => Right $ prim__resultValue val
