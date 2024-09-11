module Lib (ensure) where

import Control.Applicative (Alternative)
import Control.Monad (guard)

ensure :: (Alternative f) => (a -> Bool) -> a -> f a
ensure p a = a <$ guard (p a)