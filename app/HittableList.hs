{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module HittableList (HittableList (..), SomeHittable (..), mkHittableList) where

import Aabb (mkAabb)
import Control.Applicative
import Data.List (foldl')
import FoldHittable (FoldHittable (..))
import HitRecord (HitRecord (..))
import Hittable (Hittable (..), SomeHittable (..))
import Interval (Interval (..))
import Material (SomeMaterial)
import Object (Object (..), SomeObject)
import Ray (Ray)

newtype HittableList = HittableList [SomeObject] deriving (Show)

instance FoldHittable HittableList where
  nearestHit (HittableList hittables) ray rayT = foldl' (closestHit ray rayT) Nothing hittables
    where
      closestHit :: Ray -> Interval -> Maybe (HitRecord, SomeMaterial) -> SomeObject -> Maybe (HitRecord, SomeMaterial)
      closestHit ray (Interval tMin tMax) closestSoFar (Object object material) =
        ((,material) <$> hit object ray (Interval tMin (maybe tMax (t . fst) closestSoFar)))
          <|> closestSoFar
  combinedBoundingBox = const mkAabb

mkHittableList :: [SomeObject] -> HittableList
mkHittableList = HittableList
