module Duskull.Trace

import Data.String

public export
interface Debug s where
    debug : s -> String

export
Debug String where
    debug = id

export
Debug Int where
    debug = cast

export
Debug Integer where
    debug = cast

export
Debug Char where
    debug = cast

export
Debug Double where
    debug = cast

export
Debug Nat where
    debug = cast

export
Debug Int8 where
    debug = cast

export
Debug Int16 where
    debug = cast

export
Debug Int32 where
    debug = cast

export
Debug Int64 where
    debug = cast

export
Debug Bits8 where
    debug = cast

export
Debug Bits16 where
    debug = cast

export
Debug Bits32 where
    debug = cast

export
Debug Bits64 where
    debug = cast

export
Debug Void where
    debug _ = "Void"

export
Debug a => Debug (Maybe a) where
    debug (Just a) = "Just " ++ debug a
    debug _ = "Nothing"

export
(Debug a, Debug b) => Debug (Either a b) where
    debug (Right a) = "Right " ++ debug a
    debug (Left b) = "Left " ++ debug b

export
Debug a => Debug (List a) where
    debug xs = "[ " ++ ys ++ " ]"
        where ys : String
              ys = joinBy ", " $ map debug xs

export
printDebug : (Debug a, HasIO io) => a -> io ()
printDebug = putStr . debug

export
printLnDebug : (Debug a, HasIO io) => a -> io ()
printLnDebug = putStrLn . debug
