{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Ray (Ray (..), RayTrait (..), fromCVecs) where

import Vec3 (CVec3, Vec3 (..))

-- P(t)=A+tb
-- Where P is a 3D position along a line in 3D.
-- A is the ray origin and
-- b is the ray direction.
-- The ray parameter t is a real number

class (Vec3 (VecType r)) => RayTrait r where
  type VecType r
  fromVecs :: VecType r -> VecType r -> r
  toVecs :: r -> (VecType r, VecType r)
  toVecs r = (getOrigin r, getDestination r)
  getOrigin :: r -> VecType r
  getDestination :: r -> VecType r

  at :: r -> Double -> VecType r
  at ray t = getOrigin ray <+> (getDestination ray .^ t)

data Ray v = (Vec3 v) => Ray
  { rayOrigin :: v,
    rayDestination :: v
  }

deriving instance (Show v) => Show (Ray v)

deriving instance (Eq v) => Eq (Ray v)

instance (Vec3 v) => RayTrait (Ray v) where
  type VecType (Ray v) = v
  fromVecs :: (Vec3 v) => v -> v -> Ray v
  fromVecs rayOrigin rayDestination = Ray {rayOrigin, rayDestination}
  getOrigin :: (Vec3 v) => Ray v -> v
  getOrigin = rayOrigin
  getDestination :: (Vec3 v) => Ray v -> v
  getDestination = rayDestination

fromCVecs :: CVec3 -> CVec3 -> Ray CVec3
fromCVecs = fromVecs
