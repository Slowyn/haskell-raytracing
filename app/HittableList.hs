{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module HittableList (HittableList (..), SomeHittable (..)) where

import Control.Applicative
import FoldHittable (FoldHittable (..))
import HitRecord (HitRecord (..))
import Hittable (Hittable (..), SomeHittable (..))
import Interval (Interval (..))
import Material (SomeMaterial)
import Object (Object (..), SomeObject)
import Ray (Ray)

newtype HittableList = HittableList [SomeObject]

instance FoldHittable HittableList where
  nearestHit (HittableList objects) ray rayT = foldl (closestHit ray rayT) Nothing objects
    where
      closestHit :: Ray -> Interval -> Maybe (HitRecord, SomeMaterial) -> SomeObject -> Maybe (HitRecord, SomeMaterial)
      closestHit ray (Interval tMin tMax) closestSoFar (Object object material) =
        ((,material) <$> hit object ray (Interval tMin (maybe tMax (t . fst) closestSoFar)))
          <|> closestSoFar
