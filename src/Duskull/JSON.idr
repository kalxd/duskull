module Duskull.JSON

import Data.List
import Data.Either
import Data.String
import Control.Monad.Writer
import Control.Monad.Either
import Control.Monad.Identity
import Language.JSON
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

export
Parser : (a: Type) -> Type
Parser = EitherT String (Writer (List String))

public export
interface FromJSON a where
    fromJSON : JSON -> Parser a

export
decode : FromJSON a => String -> Either String a
decode input = do
    json <- maybeToEither "不是有效的JSON字符串！" $ parse input
    let (result, paths) = runWriter $ runEitherT $ fromJSON {a=a} json
    case result of
        Right a => pure a
        Left e => Left $ "在\{joinBy "," paths}处解析失呚：\{e}"
