{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module HittableList (HittableList (..), SomeHittable (..)) where

import Control.Applicative
import FoldHittable (FoldHittable (..))
import HitRecord (HitRecord (..))
import Hittable (Hittable (..), SomeHittable (..))
import Material (SomeMaterial)
import Object (Object (..), SomeObject)
import Ray (Ray)

newtype HittableList = HittableList [SomeObject]

instance FoldHittable HittableList where
  nearestHit (HittableList objects) ray tMin tMax = foldl (closestHit ray tMin) Nothing objects
    where
      closestHit :: Ray -> Double -> Maybe (HitRecord, SomeMaterial) -> SomeObject -> Maybe (HitRecord, SomeMaterial)
      closestHit ray tMin closestSoFar (Object object material) = ((,material) <$> hit object ray tMin (maybe tMax (t . fst) closestSoFar)) <|> closestSoFar
