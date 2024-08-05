module Hittable (Hittable (..), SomeHittable (..)) where

import HitRecord (HitRecord (..))
import Material (SomeMaterial)
import Ray (Ray)

class Hittable h where
  hit :: h -> Ray -> Double -> Double -> Maybe (HitRecord, SomeMaterial)

data SomeHittable where
  MkSomeHittable :: (Hittable a) => a -> SomeHittable
