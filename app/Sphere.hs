{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Sphere (Sphere (..), mkSphere, mkMovingSphere) where

import Aabb (Aabb, combineAabbs, mkAabb, mkAabbPoints)
import Control.Applicative
import Control.Monad
import HitRecord (createHitRecord, solveFrontFaceNorm)
import Hittable (Hittable (..))
import Interval (Interval (..))
import Ray (Ray (..), RayTrait (..))
import Vec3 (V3, Vec3 (..))

data Sphere
  = Sphere {center :: !V3, radius :: !Double, bbox :: !Aabb}
  | MovingSphere {center1 :: !V3, center2 :: !V3, centerVec :: !V3, radius :: !Double, bbox :: !Aabb}
  deriving (Show, Eq)

instance Hittable Sphere where
  hit sphere ray (Interval tMin tMax) = do
    let (center, radius) = (centerPosition sphere (time ray), getRadius sphere)
        (origin, direction) = toVecs ray
        oc = center <-> origin
        a = squareNorm direction
        h = direction .* oc
        c = squareNorm oc - radius * radius
        maybeDiscriminant = ensure (> 0) (h * h - a * c)
    discriminant <- maybeDiscriminant
    let sqrtd = sqrt discriminant
        root1 = (h - sqrtd) / a
        root2 = (h + sqrtd) / a
    root <- findClosestRoot tMin tMax root1 root2
    let t = root
        p = at ray t
        outwardNorm = (p <-> center) /^ radius
        (frontFace, normal) = solveFrontFaceNorm ray outwardNorm
    pure $ createHitRecord p normal t frontFace

  boundingBox = bbox

findClosestRoot :: Double -> Double -> Double -> Double -> Maybe Double
findClosestRoot tMin tMax root1 root2 = ensure valueInTRange root1 <|> ensure valueInTRange root2
  where
    valueInTRange = valueInRange tMin tMax

-- tmin < t < tmax
valueInRange :: Double -> Double -> Double -> Bool
valueInRange tMin tMax t = tMin < t && t < tMax

ensure :: (Alternative f) => (a -> Bool) -> a -> f a
ensure p a = a <$ guard (p a)

mkSphere :: V3 -> Double -> Sphere
mkSphere center r = Sphere {center, radius, bbox = mkAabbPoints (center <-> rVec) (center <+> rVec)}
  where
    radius = max 0 r
    rVec = fromValue radius

mkMovingSphere :: V3 -> V3 -> Double -> Sphere
mkMovingSphere center1 center2 r =
  MovingSphere
    { center1,
      center2,
      centerVec = center2 <-> center1,
      radius,
      bbox = bbox
    }
  where
    radius = max 0 r
    rVec = fromValue radius
    box1 = mkAabbPoints (center1 <-> rVec) (center1 <+> rVec)
    box2 = mkAabbPoints (center2 <-> rVec) (center2 <+> rVec)
    bbox = combineAabbs box1 box2

centerPosition :: Sphere -> Double -> V3
centerPosition (Sphere {..}) _t = center
centerPosition (MovingSphere {..}) t = center1 <+> centerVec .^ t

getRadius :: Sphere -> Double
getRadius = radius
