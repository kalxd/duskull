||| 标准库中的Monad进行扩展。
module Duskull.Control.Monad

import Control.Monad.Maybe

%default total

||| 弥补MaybeT没有liftMaybe的遗憾。
export
liftMaybe : Monad m => Maybe a -> MaybeT m a
liftMaybe = MkMaybeT . pure
