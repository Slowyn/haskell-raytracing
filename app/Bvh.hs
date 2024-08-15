module Bvh (Bvh (..), buildBvh, nearestHit, toDataTree) where

import Aabb (Aabb (..), combineAabbs, getAabbComponent, hitBox, longestAxis, mkAabb)
import Axis (Axis (..))
import Control.Applicative (Alternative (..), (<|>))
import Data.List (foldl', sortBy)
import Data.Tree (Tree (..))
import FoldHittable (FoldHittable (..))
import HitRecord (HitRecord (..))
import Hittable (Hittable (..))
import Interval (Interval (..), mkInterval)
import Object (Object (material), SomeObject)

data Bvh a
  = Leaf Aabb a
  | Branch Int Aabb (Bvh a) (Bvh a)
  | Empty
  deriving (Show, Eq)

toDataTree :: (Show a) => Bvh a -> Tree String
toDataTree Empty = Node "Empty" []
toDataTree (Leaf bbox a) = Node (show bbox) []
toDataTree (Branch _ bbox l r) = Node ("Node" ++ " " ++ show bbox) [toDataTree l, toDataTree r]

instance FoldHittable (Bvh SomeObject) where
  nearestHit bvh rayIn rayT = do
    rayT' <- hitBox (combinedBoundingBox bvh) rayIn rayT
    case bvh of
      Empty -> Nothing
      Leaf _ object -> (,material object) <$> hit object rayIn rayT'
      Branch _ _ l r -> do
        let leftHitMaybe = nearestHit l rayIn rayT'
            (Interval rMin rMax) = rayT'
            rightRayT = mkInterval rMin (maybe rMax (t . fst) leftHitMaybe)
            rightHitMaybe = nearestHit r rayIn rightRayT
        rightHitMaybe <|> leftHitMaybe

  combinedBoundingBox (Leaf bbox _) = bbox
  combinedBoundingBox (Branch _ bbox _ _) = bbox
  combinedBoundingBox Empty = mkAabb

buildBvh :: [SomeObject] -> Int -> IO (Bvh SomeObject)
buildBvh world depth = do
  let bbox = foldl' combineAabbs mkAabb (map boundingBox world)
      axis = longestAxis bbox
      cmp = boxCompare axis
      objectSpan = length world
  putStrLn $ "Split by Axis: " ++ show axis ++ "\tDepth: " ++ show depth ++ "\tObjects span: " ++ show objectSpan
  case objectSpan of
    0 -> do
      print "Zero length, put Empty"
      pure Empty
    1 -> do
      print "Length of one, put Leaf"
      let object = head world
      pure $ Leaf bbox object
    2 -> do
      print "Length of two, put Branch with leafs"
      let leftObject = world !! 0
          rightObject = world !! 1
          leftBbox = boundingBox leftObject
          rightBbox = boundingBox rightObject
      pure $ Branch depth bbox (Leaf leftBbox leftObject) (Leaf rightBbox rightObject)
    _ -> do
      print "Split list in halfs and run recrusion"
      let sortedWorld = sortBy cmp world
          (leftWorld, rightWorld) = splitlist sortedWorld
      print $ "left world length: " ++ (show . length) leftWorld ++ ", right world length: " ++ (show . length) rightWorld
      left <- buildBvh leftWorld (depth + 1)
      right <- buildBvh rightWorld (depth + 1)
      pure $ Branch depth bbox left right

boxCompare :: (Hittable a) => Axis -> a -> a -> Ordering
boxCompare axis aObject bObject = compare aMin bMin
  where
    aAxisInterval = getAabbComponent (boundingBox aObject) axis
    bAxisInterval = getAabbComponent (boundingBox bObject) axis
    (Interval aMin _) = aAxisInterval
    (Interval bMin _) = bAxisInterval

splitlist :: [a] -> ([a], [a])
splitlist xs = splitAt ((length xs + 1) `div` 2) xs
