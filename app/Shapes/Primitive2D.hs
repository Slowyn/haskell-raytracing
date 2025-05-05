module Shapes.Primitive2D (Shape2D (..), UvTrait (..), Primitive2D (..), mkStdShape) where

import Aabb (Aabb, combineAabbs, mkAabbPoints)
import HitRecord
import Hittable (Hittable (..))
import Interval (intervalContains)
import Lib (ensure)
import Ray (RayTrait (..))
import Vec3 (V3, Vec3 (..))

class UvTrait r where
  getUV :: r -> Double -> Double -> Maybe (Double, Double)

data Shape2D = Shape2D
  { unQ :: !V3,
    unU :: !V3,
    unV :: !V3,
    unW :: !V3,
    unBbox :: !Aabb,
    unNormal :: !V3,
    unD :: !Double
  }
  deriving (Show, Eq)

data Primitive2D a = Primitive2D
  { primitiveShape :: Shape2D,
    primitiveExtra :: a
  }
  deriving (Show, Eq)

instance (UvTrait r) => Hittable (Primitive2D r) where
  {-# INLINE hit #-}
  hit (Primitive2D {primitiveShape, primitiveExtra}) rayIn rayT = do
    let maybeDenom = unNormal primitiveShape .* getDirection rayIn
    denom <- ensure ((> 1e-8) . abs) maybeDenom
    let maybeT = (unD primitiveShape - unNormal primitiveShape .* getOrigin rayIn) / denom
    t <- ensure (intervalContains rayT) maybeT
    let intersection = at rayIn t
        planarHitptVector = intersection <-> unQ primitiveShape
        alpha = unW primitiveShape .* (planarHitptVector >< unV primitiveShape)
        beta = unW primitiveShape .* (unU primitiveShape >< planarHitptVector)
        (frontFace, outwardNorm) = solveFrontFaceNorm rayIn (unNormal primitiveShape)
    (u, v) <- getUV primitiveExtra alpha beta
    pure $ mkHitRecord intersection outwardNorm t frontFace u v
  boundingBox = unBbox . primitiveShape

mkStdShape :: V3 -> V3 -> V3 -> Shape2D
mkStdShape q u v =
  Shape2D
    { unQ = q,
      unU = u,
      unV = v,
      unW = w,
      unBbox = bbox,
      unNormal = normal,
      unD = d
    }
  where
    bboxDiagonal1 = mkAabbPoints q (q <+> u <+> v)
    bboxDiagonal2 = mkAabbPoints (q <+> u) (q <+> v)
    bbox = combineAabbs bboxDiagonal1 bboxDiagonal2
    n = u >< v
    normal = normalize n
    d = normal .* q
    w = n /^ (n .* n)