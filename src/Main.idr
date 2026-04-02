module Main

data Lin : Type -> Type where
    MkLin : (1 _ : a) -> Lin a

data Unr : Type -> Type where
    MkUnr : a -> Unr a

getLin : (1 _ : Lin a) -> a
getLin (MkLin x) = x

getUnr : Unr a -> a
getUnr (MkUnr x) = ?help_unr

main : IO ()
main = do
    let a = MkLin 1
        b = MkLin "hello"
    printLn $ getLin a
    printLn $ getLin a

