module Duskull.FS

import System.File

import Duskull.Error

export
readAll : HasIO io => String -> io (Either SomeError String)
readAll filepath = withFile filepath Read onError onOk
    where onError : FileError -> io SomeError
          onError = pure . ioError
          onOk : File -> io (Either SomeError String)
          onOk file = do
              Right x <- fRead file
              | Left e => pure $ Left $ ioError e
              pure $ Right x
