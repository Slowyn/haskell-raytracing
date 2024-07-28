{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Sphere (Sphere (..), mkSphere) where

import Control.Applicative
import Control.Monad
import HitRecord (createHitRecord, solveFrontFaceNorm)
import Hittable (Hittable (..))
import Material (Material, SomeMaterial (..))
import Ray (RayTrait (..))
import Vec3 (V3, Vec3 (..))

data Sphere m = (Material m) => Sphere m V3 Double

deriving instance (Show m) => Show (Sphere m)

deriving instance (Eq m) => Eq (Sphere m)

instance (Material m) => Hittable (Sphere m) where
  hit sphere ray tMin tMax = do
    let (Sphere material center radius) = sphere
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
    pure (createHitRecord p normal t frontFace, SomeMaterial material)

findClosestRoot :: Double -> Double -> Double -> Double -> Maybe Double
findClosestRoot tMin tMax root1 root2 = ensure valueInTRange root1 <|> ensure valueInTRange root2
  where
    valueInTRange = valueInRange tMin tMax

-- tmin < t < tmax
valueInRange :: Double -> Double -> Double -> Bool
valueInRange tMin tMax t = tMin < t && t < tMax

ensure :: (Alternative f) => (a -> Bool) -> a -> f a
ensure p a = a <$ guard (p a)

mkSphere :: (Material m) => m -> V3 -> Double -> Sphere m
mkSphere mat position radius = Sphere mat position (max 0 radius)
