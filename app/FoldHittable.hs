module FoldHittable (FoldHittable (..)) where

import Aabb (Aabb)
import HitRecord (HitRecord)
import Interval (Interval)
import Material (SomeMaterial)
import Ray (Ray)

class FoldHittable h where
  nearestHit :: h -> Ray -> Interval -> Maybe (HitRecord, SomeMaterial)
  combinedBoundingBox :: h -> Aabb
