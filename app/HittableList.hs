{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module HittableList (HittableList (..), SomeHittable (..), mkHittableList) where

import Aabb (Aabb, combineAabbs, mkAabb)
import Control.Applicative
import Data.List (foldl')
import FoldHittable (FoldHittable (..))
import HitRecord (HitRecord (..))
import Hittable (Hittable (..), SomeHittable (..))
import Interval (Interval (..))
import Material (SomeMaterial)
import Object (Object (..), SomeObject)
import Ray (Ray)

data HittableList = HittableList
  { objects :: ![SomeObject],
    bbox :: !Aabb
  }

instance FoldHittable HittableList where
  nearestHit hittables ray rayT = foldl' (closestHit ray rayT) Nothing objectsList
    where
      objectsList = objects hittables
      closestHit :: Ray -> Interval -> Maybe (HitRecord, SomeMaterial) -> SomeObject -> Maybe (HitRecord, SomeMaterial)
      closestHit ray (Interval tMin tMax) closestSoFar (Object object material) =
        ((,material) <$> hit object ray (Interval tMin (maybe tMax (t . fst) closestSoFar)))
          <|> closestSoFar
  combinedBoundingBox = bbox

mkHittableList :: [SomeObject] -> HittableList
mkHittableList sceneItems =
  HittableList
    { objects = sceneItems,
      bbox
    }
  where
    bbox = foldl' combineAabbs mkAabb $ map boundingBox sceneItems
