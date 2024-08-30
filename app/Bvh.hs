module Bvh (Bvh (..), buildBvh, nearestHit) where

import Aabb (Aabb (..), combineAabbs, getAabbComponent, hitBox, longestAxis, mkAabb)
import Axis (Axis (..))
import Control.Applicative (Alternative (..), (<|>))
import Data.List (foldl', sortBy)
import FoldHittable (FoldHittable (..))
import HitRecord (HitRecord (..))
import Hittable (Hittable (..))
import Interval (Interval (..), mkInterval)
import Object (Object (material), SomeObject)

data Bvh a
  = Leaf !Aabb !a
  | Branch !Int !Aabb !(Bvh a) !(Bvh a)
  | Empty
  deriving (Show, Eq)

instance FoldHittable (Bvh SomeObject) where
  nearestHit bvh rayIn rayT@(Interval tMin _tMax) = do
    rayT' <- hitBox (combinedBoundingBox bvh) rayIn rayT
    case bvh of
      Empty -> Nothing
      Leaf _ object -> (,material object) <$> hit object rayIn rayT'
      Branch _ _ l r ->
        ( do
            lHit@(HitRecord {t}, _) <- nearestHit l rayIn rayT'
            nearestHit r rayIn (mkInterval tMin t) <|> pure lHit
        )
          <|> nearestHit r rayIn rayT'

  combinedBoundingBox (Leaf bbox _) = bbox
  combinedBoundingBox (Branch _ bbox _ _) = bbox
  combinedBoundingBox Empty = mkAabb

buildBvh :: [SomeObject] -> Int -> Bvh SomeObject
buildBvh world depth = do
  let bbox = foldl' combineAabbs mkAabb (map boundingBox world)
      axis = longestAxis bbox
      cmp = boxCompare axis
      objectSpan = length world
  case objectSpan of
    0 -> Empty
    1 -> do
      let object = world !! 0
      Leaf bbox object
    2 -> do
      let leftObject = world !! 0
          rightObject = world !! 1
          leftBbox = boundingBox leftObject
          rightBbox = boundingBox rightObject
      Branch depth bbox (Leaf leftBbox leftObject) (Leaf rightBbox rightObject)
    _ -> do
      let sortedWorld = sortBy cmp world
          (leftWorld, rightWorld) = splitlist sortedWorld
          left = buildBvh leftWorld (depth + 1)
          right = buildBvh rightWorld (depth + 1)
      Branch depth bbox left right

boxCompare :: (Hittable a) => Axis -> a -> a -> Ordering
boxCompare axis aObject bObject = compare aMin bMin
  where
    aAxisInterval = getAabbComponent (boundingBox aObject) axis
    bAxisInterval = getAabbComponent (boundingBox bObject) axis
    (Interval aMin _) = aAxisInterval
    (Interval bMin _) = bAxisInterval

splitlist :: [a] -> ([a], [a])
splitlist xs = splitAt ((length xs + 1) `div` 2) xs
