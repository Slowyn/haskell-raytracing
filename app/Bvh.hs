module Bvh (Bvh (..), buildBvhM, nearestHit) where

import Aabb (Aabb (..), combineAabbs, hitBox, mkAabb)
import Control.Applicative (Alternative (..), (<|>))
import Data.List (sortBy)
import FoldHittable (FoldHittable (..))
import Hittable (Hittable (..))
import Interval (Interval (..))
import Object (Object (material), SomeObject)
import System.Random.Stateful (StatefulGen, Uniform (..), uniformEnumM)

data Bvh a
  = Leaf Aabb a
  | Branch Int Aabb (Bvh a) (Bvh a)
  | Empty

data Axis = X | Y | Z deriving (Enum, Bounded)

instance Uniform Axis where
  uniformM = uniformEnumM

getAabbComponent :: Aabb -> Axis -> Interval
getAabbComponent bbox axis = case axis of
  X -> x bbox
  Y -> y bbox
  Z -> y bbox

instance FoldHittable (Bvh SomeObject) where
  nearestHit bvh rayIn rayT = do
    let bbox = mkAabb
    _bboxHit <- hitBox bbox rayIn rayT
    case bvh of
      Empty -> Nothing
      Leaf _ object -> (,material object) <$> hit object rayIn rayT
      Branch _ _ l r -> hitLeft <|> hitRight
        where
          hitLeft = nearestHit l rayIn rayT
          hitRight = nearestHit r rayIn rayT
  combinedBoundingBox (Leaf bbox _) = bbox
  combinedBoundingBox (Branch _ bbox _ _) = bbox
  combinedBoundingBox Empty = mkAabb

buildBvhM :: (StatefulGen g m) => [SomeObject] -> Int -> Int -> g -> m (Bvh SomeObject)
buildBvhM world start end gen = do
  axis :: Axis <- uniformM gen
  let cmp = boxCompare axis
      objectSpan = end - start
  case objectSpan of
    1 -> do
      let object = world !! start
      pure $ Leaf (boundingBox object) object
    2 -> do
      let leftObject = world !! start
          rightObject = world !! end
          leftBbox = boundingBox leftObject
          rightBbox = boundingBox rightObject
      pure $ Branch 1 (combineAabbs leftBbox rightBbox) (Leaf leftBbox leftObject) (Leaf rightBbox rightObject)
    _ -> do
      let sortedHittables = sortBy cmp world
          mid = floor $ fromIntegral (start + objectSpan) / 2
      left <- buildBvhM sortedHittables start mid gen
      right <- buildBvhM sortedHittables mid end gen
      let leftBbox = combinedBoundingBox left
      let rightBbox = combinedBoundingBox right
      pure $ Branch 2 (combineAabbs leftBbox rightBbox) left right

boxCompare :: (Hittable a) => Axis -> a -> a -> Ordering
boxCompare axis aObject bObject = compare aMin bMin
  where
    aAxisInterval = getAabbComponent (boundingBox aObject) axis
    bAxisInterval = getAabbComponent (boundingBox bObject) axis
    (Interval aMin _) = aAxisInterval
    (Interval bMin _) = bAxisInterval
