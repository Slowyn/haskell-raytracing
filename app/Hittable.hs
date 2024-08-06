module Hittable (Hittable (..), SomeHittable (..)) where

import HitRecord (HitRecord (..))
import Interval (Interval)
import Ray (Ray)

class Hittable h where
  hit :: h -> Ray -> Interval -> Maybe HitRecord

data SomeHittable where
  MkSomeHittable :: (Hittable a) => a -> SomeHittable

instance Hittable SomeHittable where
  hit (MkSomeHittable obj) = hit obj
