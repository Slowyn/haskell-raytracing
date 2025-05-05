module Shapes.Tri (Tri (..), mkTri) where

import Control.Monad (guard)
import Interval (intervalContains, mkInterval)
import Lib (ensure)
import Shapes.Primitive2D (Primitive2D (..), UvTrait (..), mkStdShape)
import Vec3 (V3)

data Tri = Tri deriving (Show, Eq)

instance UvTrait Tri where
  getUV _ alpha beta = do
    let unitinterval = mkInterval 0 1
    u <- ensure (intervalContains unitinterval) alpha
    v <- ensure (intervalContains unitinterval) beta
    guard (alpha + beta <= 1)
    pure (u, v)

mkTri :: V3 -> V3 -> V3 -> Primitive2D Tri
mkTri q u v =
  Primitive2D
    { primitiveExtra = Tri,
      primitiveShape = mkStdShape q u v
    }