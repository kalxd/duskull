module Duskull.FFI

public export
loadlib : String -> String
loadlib name = "C:" ++ name ++ ",libgolbat"

||| 从Rust返回的Result指针类型。
export
data Result : Type -> Type where
