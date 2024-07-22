{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Hittable (HitRecord, Hittable (..), createHitRecord, solveFrontFaceNorm) where

import Ray (Ray, RayTrait (getDirection))
import Vec3

data HitRecord v = (Vec3 v) => HitRecord
  { p :: v,
    normal :: v,
    t :: Double,
    frontFace :: Bool
  }

solveFrontFaceNorm :: (Vec3 v) => Ray v -> v -> (Bool, v)
solveFrontFaceNorm ray outwardNorm = if frontFace then (frontFace, outwardNorm) else (frontFace, invert outwardNorm)
  where
    frontFace = getDirection ray .* outwardNorm < 0

createHitRecord :: (Vec3 v) => v -> v -> Double -> Bool -> HitRecord v
createHitRecord p normal t frontFace = HitRecord {p, normal, t, frontFace}

class (Vec3 (VecType h)) => Hittable h where
  type VecType h
  hit :: h -> Ray (VecType h) -> Double -> Double -> Maybe (HitRecord (VecType h))