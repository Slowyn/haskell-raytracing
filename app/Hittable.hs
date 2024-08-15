module Hittable (Hittable (..), SomeHittable (..)) where

import Aabb (Aabb)
import HitRecord (HitRecord (..))
import Interval (Interval)
import Ray (Ray)

class Hittable h where
  hit :: h -> Ray -> Interval -> Maybe HitRecord
  boundingBox :: h -> Aabb

data SomeHittable where
  MkSomeHittable :: (Hittable a, Show a) => a -> SomeHittable

instance Hittable SomeHittable where
  hit (MkSomeHittable obj) = hit obj
  boundingBox (MkSomeHittable obj) = boundingBox obj

instance Show SomeHittable where
  show (MkSomeHittable object) = show object
