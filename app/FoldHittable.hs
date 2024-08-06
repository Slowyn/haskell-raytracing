module FoldHittable (FoldHittable (..)) where

import HitRecord (HitRecord)
import Interval (Interval)
import Material (SomeMaterial)
import Ray (Ray)

class FoldHittable h where
  nearestHit :: h -> Ray -> Interval -> Maybe (HitRecord, SomeMaterial)