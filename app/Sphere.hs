{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Sphere (Sphere (..)) where

import Control.Applicative
import Control.Monad
import Hittable (HitRecord, Hittable (..), createHitRecord, solveFrontFaceNorm)
import Ray (Ray, RayTrait (..))
import Vec3 (Vec3 (..))

data Sphere v = (Vec3 v) => Sphere v Double

deriving instance (Show v) => Show (Sphere v)

deriving instance (Eq v) => Eq (Sphere v)

instance (Vec3 v) => Hittable (Sphere v) where
  type VecType (Sphere v) = v

  hit :: Sphere v -> Ray v -> Double -> Double -> Maybe (HitRecord v)
  hit sphere ray tMin tMax = do
    let (Sphere center radius) = sphere
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
    Just $ createHitRecord p normal t frontFace

findClosestRoot :: Double -> Double -> Double -> Double -> Maybe Double
findClosestRoot tMin tMax root1 root2 = ensure valueInTRange root1 <|> ensure valueInTRange root2
  where
    valueInTRange = valueInRange tMin tMax

-- tmin < t < tmax
valueInRange :: Double -> Double -> Double -> Bool
valueInRange tMin tMax t = tMin < t && t < tMax

ensure :: (Alternative f) => (a -> Bool) -> a -> f a
ensure p a = a <$ guard (p a)