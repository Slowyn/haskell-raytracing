module Axis (Axis (..)) where

import System.Random.Stateful (Uniform, uniformEnumM, uniformM)

data Axis = X | Y | Z deriving (Enum, Bounded, Show)

instance Uniform Axis where
  uniformM = uniformEnumM
