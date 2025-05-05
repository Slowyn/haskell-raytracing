module Shapes.Tri (Tri (..), mkTri) where

import Control.Monad (guard)
import Shapes.Primitive2D (Primitive2D (..), UvTrait (..), mkStdShape)
import Vec3 (V3)

data Tri = Tri deriving (Show, Eq)

instance UvTrait Tri where
  getUV _ alpha beta = do
    guard (alpha >= 0 && beta >= 0 && alpha + beta <= 1)
    pure (alpha, beta)

mkTri :: V3 -> V3 -> V3 -> Primitive2D Tri
mkTri q u v =
  Primitive2D
    { primitiveExtra = Tri,
      primitiveShape = mkStdShape q u v
    }