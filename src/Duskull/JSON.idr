module Duskull.JSON

import Language.JSON.Data

interface ToJSON a where
    toJSON : a -> JSON

ToJSON JSON where
    toJSON = id

ToJSON a => ToJSON (List a) where
    toJSON = JArray . map toJSON

ToJSON String where
    toJSON = JString

ToJSON Double where
    toJSON = JNumber
