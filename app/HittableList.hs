{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module HittableList (HittableList (..), AnyHittable (..)) where

import Control.Applicative
import Hittable (HitRecord (..), Hittable (..))
import Ray hiding (VecType)
import Vec3 (Vec3 (..))

-- | Wrapper to organize list of any hittable object
data AnyHittable v = forall a. (Hittable a, VecType a ~ v) => AnyHittable a

newtype HittableList v = HittableList [AnyHittable v]

instance (Vec3 v) => Hittable (HittableList v) where
  type VecType (HittableList v) = v

  hit :: HittableList v -> Ray v -> Double -> Double -> Maybe (HitRecord v)
  hit (HittableList objects) ray tMin tMax = foldl (closestHit ray tMin) Nothing objects
    where
      closestHit :: Ray v -> Double -> Maybe (HitRecord v) -> AnyHittable v -> Maybe (HitRecord v)
      closestHit ray tMin closestSoFar (AnyHittable object) = hit object ray tMin (maybe tMax t closestSoFar) <|> closestSoFar
