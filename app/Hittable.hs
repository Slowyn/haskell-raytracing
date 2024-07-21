{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}

module Hittable (HitRecord, Hittable (..), createHitRecord) where

import Ray (Ray)
import Vec3

data HitRecord v = (Vec3 v) => HitRecord
  { p :: v,
    normal :: v,
    t :: Double
  }

createHitRecord :: (Vec3 v) => v -> v -> Double -> HitRecord v
createHitRecord p normal t = HitRecord {p, normal, t}

class (Vec3 (VecType h)) => Hittable h where
  type VecType h
  hit :: h -> Ray (VecType h) -> Double -> Double -> Maybe (HitRecord (VecType h))