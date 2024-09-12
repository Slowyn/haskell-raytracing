module Shapes.Quad (Quad (..), mkQuad) where

import Aabb (Aabb (..), combineAabbs, mkAabbPoints)
import HitRecord (mkHitRecord, solveFrontFaceNorm)
import Hittable (Hittable (..))
import Interval (intervalContains, mkInterval)
import Lib (ensure)
import Ray (RayTrait (..), at)
import Vec3 (V3, Vec3 (..))

data Quad = Quad
  { unQ :: !V3,
    unU :: !V3,
    unV :: !V3,
    unW :: !V3,
    unBbox :: !Aabb,
    unNormal :: !V3,
    unD :: !Double
  }
  deriving (Show, Eq)

instance Hittable Quad where
  {-# INLINE hit #-}
  hit quad rayIn rayT = do
    let maybeDenom = unNormal quad .* getDirection rayIn
    denom <- ensure ((> 1e-8) . abs) maybeDenom
    let maybeT = (unD quad - unNormal quad .* getOrigin rayIn) / denom
    t <- ensure (intervalContains rayT) maybeT
    let intersection = at rayIn t
        planarHitptVector = intersection <-> unQ quad
        alpha = unW quad .* (planarHitptVector >< unV quad)
        beta = unW quad .* (unU quad >< planarHitptVector)
        (frontFace, outwardNorm) = solveFrontFaceNorm rayIn (unNormal quad)
    (u, v) <- getQuadUV alpha beta
    pure $ mkHitRecord intersection outwardNorm t frontFace u v
  boundingBox = unBbox

-- is interrior?
getQuadUV :: Double -> Double -> Maybe (Double, Double)
getQuadUV alpha beta = do
  let unitInterval = mkInterval 0 1
  u <- ensure (intervalContains unitInterval) alpha
  v <- ensure (intervalContains unitInterval) beta
  pure (u, v)

mkQuad :: V3 -> V3 -> V3 -> Quad
mkQuad q u v = Quad {unQ = q, unU = u, unV = v, unW = w, unBbox = bbox, unNormal = normal, unD = d}
  where
    bboxDiagonal1 = mkAabbPoints q (q <+> u <+> v)
    bboxDiagonal2 = mkAabbPoints (q <+> u) (q <+> v)
    bbox = combineAabbs bboxDiagonal1 bboxDiagonal2
    n = u >< v
    normal = normalize n
    d = normal .* q
    w = n /^ (n .* n)
