module Main

import Duskull.Trace
import Duskull.JSON

record User where
    constructor MkUser
    name : String
    age : Double

rawInput : String
rawInput = ##"""
{"name": "hello world"}
"""##

FromJSON User where
    fromJSON = withObject "user" $ \o => do
        name <- o .: "name"
        age <- o .:? "age" .:= 10.0
        pure $ MkUser name age

Debug User where
    debug (MkUser name age) = "User: \{debug name}, \{debug age} "

main : IO ()
main = printLnDebug $ decode {a=User} rawInput
