module Duskull.Scraper.Result

import Duskull.FFI

data CResult : Type -> Type where

data User : Type where

Result : Type
Result = CResult (Ptr User)

%foreign (loadlib "result_test")
prim__resultTest : PrimIO (Ptr Result)

%foreign (loadlib "result_is_error")
prim__resultIsError : Ptr Result -> Bits8

%foreign (loadlib "result_err_msg")
prim__resultErrorMsg : (1 _: Ptr Result) -> String

%foreign (loadlib "result_value")
prim__resultValue : (1 _: Ptr Result) -> Ptr User

%foreign (loadlib "result_print_user")
prim__resultPrintUser : (Ptr User) -> PrimIO ()

getTestTarget : IO (Ptr Result)
getTestTarget = primIO prim__resultTest

getTestUser : IO (Either String (Ptr User))
getTestUser = do val <- getTestTarget
                 pure $ case prim__resultIsError val of
                             1 => Left $ prim__resultErrorMsg val
                             _ => Right $ prim__resultValue val

printUser : (Ptr User) -> IO ()
printUser = primIO . prim__resultPrintUser

main : IO ()
main = do user <- getTestUser
          case user of
               Left err => putStrLn $ "Error: " ++ err
               Right user => printUser user
