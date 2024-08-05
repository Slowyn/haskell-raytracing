{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Sphere (Sphere (..), mkSphere, mkMovingSphere) where

import Control.Applicative
import Control.Monad
import HitRecord (createHitRecord, solveFrontFaceNorm)
import Hittable (Hittable (..))
import Ray (Ray (..), RayTrait (..))
import Vec3 (V3, Vec3 (..))

data Sphere
  = Sphere !V3 !Double
  | MovingSphere !V3 !V3 !V3 !Double
  deriving (Show, Eq)

instance Hittable Sphere where
  hit sphere ray tMin tMax = do
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
mkSphere position radius = Sphere position (max 0 radius)

mkMovingSphere :: V3 -> V3 -> Double -> Sphere
mkMovingSphere center1 center2 radius =
  MovingSphere
    center1
    center2
    (center2 <-> center1)
    (max 0 radius)

centerPosition :: Sphere -> Double -> V3
centerPosition (Sphere center _radius) _t = center
centerPosition (MovingSphere center1 _center2 centerVec _radius) t = center1 <+> centerVec .^ t

getRadius :: Sphere -> Double
getRadius (Sphere _center radius) = radius
getRadius (MovingSphere _center1 _center2 _centerVec radius) = radius
