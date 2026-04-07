module Duskull.Scraper.Element

import Data.Maybe

import Duskull.FFI
import Duskull.Error
import Duskull.Scraper.Selector

%default total

data SelectPtr : Type where

record Select where
    constructor MkSelect
    ptr : Ptr SelectPtr

export
data ElementPtr : Type where

public export
record Element where
    constructor MkElement
    ptr : GCPtr ElementPtr

%foreign (loadlib "element_free")
prim__elementFree : Ptr ElementPtr -> PrimIO ()

%foreign (loadlib "element_id")
prim__elementId : GCPtr ElementPtr -> Ptr String

%foreign (loadlib "element_attr")
prim__elementAttr : String -> GCPtr ElementPtr -> Ptr String

%foreign loadlib "element_select_free"
prim__elementSelectFree : Ptr SelectPtr -> PrimIO ()

freeSelect : HasIO io => Ptr SelectPtr -> io ()
freeSelect ptr = primIO $ prim__elementSelectFree ptr

%foreign (loadlib "element_select")
prim__elementSelect : GCPtr SelectorPtr -> GCPtr ElementPtr -> Ptr SelectPtr

%foreign (loadlib "element_select_next")
prim__elementSelectNext : Ptr SelectPtr -> Ptr ElementPtr

%foreign loadlib "element_text"
prim__elementText : GCPtr ElementPtr -> Ptr String

export
elementText : Element -> Maybe String
elementText (MkElement ptr) = castMaybeString $ prim__elementText ptr

export
elementText' : Element -> String
elementText' = fromMaybe "" . elementText

export
free : Ptr ElementPtr -> IO ()
free = primIO . prim__elementFree

export
castToElement : Ptr ElementPtr -> Element
castToElement ptr = MkElement $ unsafePerformIO $ onCollect ptr free

export
elementId : Element -> Maybe String
elementId (MkElement ptr) = castMaybeString $ prim__elementId ptr

export
elementAttr : String -> Element -> Maybe String
elementAttr css (MkElement ptr) = castMaybeString $ prim__elementAttr css ptr

export
elementHref : Element -> Maybe String
elementHref = elementAttr "href"

export
elementSrc : Element -> Maybe String
elementSrc = elementAttr "src"

covering
reduceSelectToList : List Element -> Ptr SelectPtr -> List Element
reduceSelectToList acc ptr = do
    let item = prim__elementSelectNext ptr
    if prim__nullPtr item == 1
       then acc
       else let el = castToElement item
            in reduceSelectToList (acc ++ [el]) ptr

covering
export
select : String -> Element -> Either SomeError (List Element)
select css (MkElement elementPtr) =
    let Right (MkSelector selectorPtr) = mkSelector css
        | Left e => Left e
        selectPtr = prim__elementSelect selectorPtr elementPtr
        xs = reduceSelectToList [] selectPtr
    in unsafePerformIO $ freeSelect selectPtr $> Right xs

export
select1 : String -> Element -> Either SomeError (Maybe Element)
select1 css (MkElement elementPtr) = do
    let Right (MkSelector selectorPtr) = mkSelector css
        | Left e => Left e
        selectPtr = prim__elementSelect selectorPtr elementPtr
        item = prim__elementSelectNext selectPtr
    if prim__nullPtr item == 1
       then unsafePerformIO $ freeSelect selectPtr $> Right Nothing
       else let el = Right . Just $ castToElement item
            in unsafePerformIO $ freeSelect selectPtr $> el
