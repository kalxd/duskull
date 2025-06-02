module Duskull.JSON

import Language.JSON.Data

public export
interface ToJSON (0 a: Type) where
    toJSON : a -> JSON

ToJSON JSON where
    toJSON = id

ToJSON String where
    toJSON = JString

ToJSON Double where
    toJSON = JNumber

ToJSON Integer where
    toJSON = toJSON . cast {to=Double}

ToJSON a => ToJSON (List a) where
    toJSON = JArray . map toJSON

ToJSON a => ToJSON (Maybe a) where
    toJSON (Just a) = toJSON a
    toJSON _ = JNull

public export
data JSONPair : Type where
     (.=) : ToJSON a => String -> a -> JSONPair

infix 7 .=

export
object : List JSONPair -> JSON
object = JObject . (map $ \((.=) k v) => (k, toJSON v))

export
emptyArray : JSON
emptyArray = JArray []

public export
interface FromJSON a where
    fromJSON : JSON -> a
