module Duskull.Error

%default total

export
data SomeError : Type where
    IOError : String -> SomeError
    OtherError : String -> SomeError
    ParseError : String -> SomeError

export
Show SomeError where
    show (IOError str) = "IOError: " ++ str
    show (OtherError str) = "OtherError: " ++ str
    show (ParseError str) = "ParseError: " ++ str

export
ioError : Show e => e -> SomeError
ioError = IOError . show

export
otherError : Show e => e -> SomeError
otherError = OtherError . show

export
parseError : Show e => e -> SomeError
parseError = ParseError . show
