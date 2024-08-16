{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Ray (Ray (..), RayTrait (..), mkRay) where

import Vec3 (V3, Vec3 (..))

-- P(t)=A+tb
-- Where P is a 3D position along a line in 3D.
-- A is the ray origin and
-- b is the ray direction.
-- The ray parameter t is a real number

class RayTrait r where
  fromVecs :: V3 -> V3 -> r
  toVecs :: r -> (V3, V3)
  toVecs r = (getOrigin r, getDirection r)
  getOrigin :: r -> V3
  getDirection :: r -> V3

  at :: r -> Double -> V3
  at ray t = getOrigin ray <+> (getDirection ray .^ t)

data Ray = Ray
  { rayOrigin :: !V3,
    rayDirection :: !V3,
    time :: !Double
  }
  deriving (Show, Eq)

instance RayTrait Ray where
  fromVecs :: V3 -> V3 -> Ray
  fromVecs rayOrigin rayDirection = Ray {rayOrigin, rayDirection, time = 0}
  getOrigin :: Ray -> V3
  getOrigin = rayOrigin
  getDirection :: Ray -> V3
  getDirection = rayDirection

mkRay :: V3 -> V3 -> Double -> Ray
mkRay origin direction time = Ray {rayOrigin = origin, rayDirection = direction, time}
