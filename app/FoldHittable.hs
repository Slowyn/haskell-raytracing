module FoldHittable (FoldHittable (..)) where

import HitRecord (HitRecord)
import Material (SomeMaterial)
import Ray (Ray)

class FoldHittable h where
  nearestHit :: h -> Ray -> Double -> Double -> Maybe (HitRecord, SomeMaterial)