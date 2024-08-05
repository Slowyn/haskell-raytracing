{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module HittableList (HittableList (..), SomeHittable (..)) where

import Control.Applicative
import HitRecord (HitRecord (..))
import Hittable (Hittable (..), SomeHittable (..))
import Material (SomeMaterial)
import Ray (Ray)

newtype HittableList = HittableList [SomeHittable]

instance Hittable HittableList where
  hit (HittableList objects) ray tMin tMax = foldl (closestHit ray tMin) Nothing objects
    where
      closestHit :: Ray -> Double -> Maybe (HitRecord, SomeMaterial) -> SomeHittable -> Maybe (HitRecord, SomeMaterial)
      closestHit ray tMin closestSoFar (SomeHittable object) = hit object ray tMin (maybe tMax (t . fst) closestSoFar) <|> closestSoFar
