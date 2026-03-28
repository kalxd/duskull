module Duskull.FFI

%default total

public export
loadlib : String -> String
loadlib name = "C:" ++ name ++ ",libgolbat"

||| 从Rust返回的Result指针类型。
||| 错误信息只携带String，需要根据场合自行提升错误。
export
data Result : Type -> Type where
