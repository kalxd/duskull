module Duskull.JSON

import Language.JSON.Data

interface ToJSON a where
    toJSON : a -> JSON
