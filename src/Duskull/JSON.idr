module Duskull.JSON

import Language.JSON.Data

public export
interface ToJSON (0 a: Type) where
    toJSON : a -> JSON

ToJSON JSON where
    toJSON = id

ToJSON a => ToJSON (List a) where
    toJSON = JArray . map toJSON

ToJSON String where
    toJSON = JString

ToJSON Double where
    toJSON = JNumber

ToJSON Integer where
    toJSON = toJSON . cast {to=Double}

public export
data JSONPair : Type where
     (.=) : ToJSON a => String -> a -> JSONPair

infix 7 .=

export
object : List JSONPair -> JSON
object = JObject . (map $ \((.=) k v) => (k, toJSON v))
