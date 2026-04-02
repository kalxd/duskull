module Duskull.Error

%default total

export
data SomeError : Type where
    IOError : String -> SomeError
    OtherError : String -> SomeError

export
ioError : Show e => e -> SomeError
ioError = IOError . show

export
otherError : Show e => e -> SomeError
otherError = OtherError . show
