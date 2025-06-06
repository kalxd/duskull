module Main

import Duskull.Trace
import Duskull.JSON

main : IO ()
main = printLnDebug $ decode {a=String} "[]"
