{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}

module Ray (Ray, RayTrait (..)) where

import Vec3 (Vec3 (..))

-- P(t)=A+tb
-- Where P is a 3D position along a line in 3D.
-- A is the ray origin and
-- b is the ray direction.
-- The ray parameter t is a real number

class (Vec3 v) => RayTrait r v where
  fromVecs :: v -> v -> r
  getOrigin :: r -> v
  getDestination :: r -> v

  at :: r -> Double -> v
  at ray t = getOrigin ray <+> (getDestination ray .^ t)

data Ray v = (Vec3 v) => Ray
  { rayOrigin :: v,
    rayDestination :: v
  }

deriving instance (Show v) => Show (Ray v)

deriving instance (Eq v) => Eq (Ray v)

instance (Vec3 v) => RayTrait (Ray v) v where
  fromVecs :: (Vec3 v) => v -> v -> Ray v
  fromVecs rayOrigin rayDestination = Ray {rayOrigin, rayDestination}
  getOrigin :: (Vec3 v) => Ray v -> v
  getOrigin = rayOrigin
  getDestination :: (Vec3 v) => Ray v -> v
  getDestination = rayDestination
