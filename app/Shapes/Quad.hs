module Shapes.Quad (Quad (..), mkQuad, mkBox) where

import Interval (intervalContains, mkInterval)
import Lib (ensure)
import Shapes.Primitive2D (Primitive2D (..), UvTrait (..), mkStdShape)
import Vec3 (V3, Vec3 (fromXYZ, invert, toXYZ))

data Quad = Quad deriving (Show, Eq)

instance UvTrait Quad where
  getUV _ alpha beta = do
    let unitInterval = mkInterval 0 1
    u <- ensure (intervalContains unitInterval) alpha
    v <- ensure (intervalContains unitInterval) beta
    pure (u, v)

mkQuad :: V3 -> V3 -> V3 -> Primitive2D Quad
mkQuad q u v =
  Primitive2D
    { primitiveExtra = Quad,
      primitiveShape = mkStdShape q u v
    }

mkBox :: V3 -> V3 -> [Primitive2D Quad]
mkBox pointA pointB = [front, right, back, left, top, bottom]
  where
    (ax, ay, az) = toXYZ pointA
    (bx, by, bz) = toXYZ pointB
    (minx, miny, minz) = (min ax bx, min ay by, min az bz)
    (maxx, maxy, maxz) = (max ax bx, max ay by, max az bz)
    dx = fromXYZ (maxx - minx, 0, 0)
    dy = fromXYZ (0, maxy - miny, 0)
    dz = fromXYZ (0, 0, maxz - minz)
    front = mkQuad (fromXYZ (minx, miny, maxz)) dx dy
    right = mkQuad (fromXYZ (maxx, miny, maxz)) (invert dz) dy
    back = mkQuad (fromXYZ (maxx, miny, minz)) (invert dx) dy
    left = mkQuad (fromXYZ (minx, miny, minz)) dz dy
    top = mkQuad (fromXYZ (minx, maxy, maxz)) dx (invert dz)
    bottom = mkQuad (fromXYZ (minx, miny, minz)) dx dz