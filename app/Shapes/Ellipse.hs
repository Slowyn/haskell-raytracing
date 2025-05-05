module Shapes.Ellipse (Ellipse (..), mkEllipse) where

import Control.Monad (guard)
import Shapes.Primitive2D (Primitive2D (..), UvTrait (..), mkStdShape)
import Vec3 (V3)

data Ellipse = Ellipse deriving (Show, Eq)

instance UvTrait Ellipse where
  getUV _ alpha beta = do
    let u = alpha - 0.5
        v = beta - 0.5
        r = 0.5 * 0.5
    guard $ u * u + v * v <= r
    pure (alpha, beta)

mkEllipse :: V3 -> V3 -> V3 -> Primitive2D Ellipse
mkEllipse q u v =
  Primitive2D
    { primitiveExtra = Ellipse,
      primitiveShape = mkStdShape q u v
    }
