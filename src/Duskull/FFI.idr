module Duskull.FFI

%default total

public export
loadlib : String -> String
loadlib name = "C:" ++ name ++ ",libgolbat"

||| 从Rust返回的Result指针类型。
||| 错误信息只携带String，需要根据场合自行提升错误。
export
data Result : Type -> Type where

%foreign (loadlib "result_is_error")
prim__resultIsError : Ptr (Result AnyPtr) -> Bits8

%foreign (loadlib "result_err_msg")
prim__resultErrorMsg : Ptr (Result AnyPtr) -> String

%foreign (loadlib "result_value")
prim__resultValue : Ptr (Result AnyPtr) -> AnyPtr

%foreign (loadlib "to_str")
prim__toStr : Ptr String -> String

export
unpackResult : Ptr (Result AnyPtr) -> Either String (Ptr a)
unpackResult val =
    case prim__resultIsError $ val of
        1 => Left $ prim__resultErrorMsg val
        _ => Right $ prim__castPtr $ prim__resultValue val

export
castToString : Ptr String -> String
castToString = prim__toStr

export
castMaybeString : Ptr String -> Maybe String
castMaybeString ptr =
    if prim__nullPtr ptr == 1
       then Nothing
       else Just $ prim__toStr ptr
