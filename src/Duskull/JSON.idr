module Duskull.JSON

import Data.List
import Data.Either
import Data.String
import Control.Monad.Writer
import Control.Monad.Either
import Control.Monad.Identity
import Language.JSON
import Language.JSON.Data

import public Language.JSON as Duskull.JSON
import public Language.JSON.Data as Duskull.JSON
import public Control.Monad.Either as Duskull.JSON
import public Control.Monad.Writer as Duskull.JSON
import public Control.Monad.Identity as Duskull.JSON

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
Parser : (a: Type) -> Type
Parser = EitherT String (Writer (List String))

public export
interface FromJSON a where
    fromJSON : JSON -> Parser a

export
FromJSON String where
    fromJSON (JString s) = pure s
    fromJSON _ = throwE "不是有效字符串。"

export
FromJSON Double where
    fromJSON (JNumber n) = pure n
    fromJSON _ = throwE "不是有效数字。"

export
FromJSON a => FromJSON (List a) where
    fromJSON (JArray xs) = traverse fromJSON xs
    fromJSON _ = throwE "不是有效数组。"

export
FromJSON a => FromJSON (Maybe a) where
    fromJSON JNull = pure Nothing
    fromJSON x = Just <$> fromJSON x

fromJSONEither : FromJSON a => JSON -> Either String a
fromJSONEither json = let (result, paths) = runWriter $ runEitherT $ fromJSON json
                      in case result of
                          Right a => pure a
                          Left e => Left $ case paths of
                              [] => "解板失败：\{e}"
                              _ => "在\{joinBy "," paths}解析失败：\{e}"

export
decode : FromJSON a => String -> Either String a
decode input = do
    json <- maybeToEither "不是有效JSON字符串！" $ parse input
    fromJSONEither json

export
decode' : FromJSON a => String -> Maybe a
decode' = eitherToMaybe . decode

infixl 6 .:
export
(.:) : FromJSON a => List (String, JSON) -> String -> Parser a
obj .: key = case lookup key obj of
    Just a => fromJSON a
    _ => throwE "不存在\{key}的键！"

infixl 6 .:?
export
(.:?) : FromJSON a => List (String, JSON) -> String -> Parser (Maybe a)
obj .:? key = Just <$> ((.:) obj key) `catchE` (\_ => pure Nothing)

infixl 5 .:=
export
(.:=) : Parser (Maybe a) -> a -> Parser a
ma .:= a = case !ma of
    Just x => pure x
    Nothing => pure a

export
withObject : FromJSON a => String -> (List (String, JSON) -> Parser a) -> JSON -> Parser a
withObject path p json = do
    tell [path]
    case json of
        JObject xs => p xs
        _ => throwE "无效的Object!"

record User where
    constructor MkUser
    name : String
    age : Double

FromJSON User where
    fromJSON = withObject "user" $ \o => do
        name <- o .: "name"
        age <- o .: "age"
        pure $ MkUser name age
