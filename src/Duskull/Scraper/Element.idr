module Duskull.Scraper.Element

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

%foreign (loadlib "element_select")
prim__elementSelect : GCPtr SelectorPtr -> GCPtr ElementPtr -> Ptr SelectPtr

%foreign (loadlib "element_select_next")
prim__elementSelectNext : Ptr SelectPtr -> Ptr ElementPtr

export
free : Ptr ElementPtr -> IO ()
free = primIO . prim__elementFree

export
castToElement : HasIO io => Ptr ElementPtr -> io Element
castToElement ptr = MkElement <$> onCollect ptr free

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
reduceSelectToList : HasIO io => List Element -> Ptr SelectPtr -> io (List Element)
reduceSelectToList acc ptr = do
    let item = prim__elementSelectNext ptr
    if prim__nullPtr item == 1
       then pure acc
       else do
           el <- castToElement item
           reduceSelectToList (acc ++ [el]) ptr

covering
export
select : String -> Element -> IO (Either SomeError (List Element))
select css (MkElement elementPtr) = do
    selector <- mkSelector css
    case selector of
        Left e => pure $ Left e
        Right (MkSelector selectorPtr) => do
            let selectPtr = prim__elementSelect selectorPtr elementPtr
            Right <$> reduceSelectToList [] selectPtr

export
select1 : String -> Element -> IO (Either SomeError (Maybe Element))
select1 css (MkElement elementPtr) = do
    selector <- mkSelector css
    case selector of
        Left e => pure $ Left e
        Right (MkSelector selectorPtr) => do
            let selectPtr = prim__elementSelect selectorPtr elementPtr
                item = prim__elementSelectNext selectPtr
            if prim__nullPtr item == 1
               then pure $ Right Nothing
               else Right . Just <$> castToElement item
