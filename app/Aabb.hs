module Aabb
  ( Aabb (..),
    mkAabb,
    mkAabbIntervals,
    mkAabbPoints,
    combineAabbs,
    hitBox,
    longestAxis,
    getAabbComponent,
  )
where

import Axis (Axis (..))
import Control.Applicative
import Data.Foldable (maximumBy)
import Interval (Interval (..), defaultInterval, expandInterval, intervalSize, mkInterval, mkInterval')
import Ray (Ray, RayTrait (..))
import Vec3 qualified as V

data Aabb = Aabb {x :: !Interval, y :: !Interval, z :: !Interval} deriving (Show, Eq)

defaultAabb :: Aabb
defaultAabb = Aabb {x = defaultInterval, y = defaultInterval, z = defaultInterval}

mkAabb :: Aabb
mkAabb = defaultAabb

mkAabbIntervals :: Interval -> Interval -> Interval -> Aabb
mkAabbIntervals xInterval yInterval zInterval =
  Aabb
    { x = padToMinimum xInterval,
      y = padToMinimum yInterval,
      z = padToMinimum zInterval
    }

mkAabbPoints :: V.V3 -> V.V3 -> Aabb
mkAabbPoints a b = mkAabbIntervals xInterval yInterval zInterval
  where
    xInterval = if V.x a <= V.x b then mkInterval (V.x a) (V.x b) else mkInterval (V.x b) (V.x a)
    yInterval = if V.y a <= V.y b then mkInterval (V.y a) (V.y b) else mkInterval (V.y b) (V.y a)
    zInterval = if V.z a <= V.z b then mkInterval (V.z a) (V.z b) else mkInterval (V.z b) (V.z a)

combineAabbs :: Aabb -> Aabb -> Aabb
combineAabbs b1 b2 =
  Aabb
    { x = combinedXInterval,
      y = combinedYInterval,
      z = combinedZInterval
    }
  where
    combinedXInterval = mkInterval' (x b1) (x b2)
    combinedYInterval = mkInterval' (y b1) (y b2)
    combinedZInterval = mkInterval' (z b1) (z b2)

hitBox :: Aabb -> Ray -> Interval -> Maybe Interval
hitBox bbox rayIn (Interval rayTmin rayTmax) = do
  let (origin, direction) = toVecs rayIn
      go :: Interval -> (V.V3 -> Double) -> Maybe Interval
      go (Interval aMin aMax) comp = do
        let adinv = 1.0 / comp direction
            t0 = (aMin - comp origin) * adinv
            t1 = (aMax - comp origin) * adinv
            outputT@(Interval ta tb)
              | t0 < t1 = Interval (max rayTmin t0) (min rayTmax t1)
              | otherwise = Interval (max rayTmin t1) (min rayTmax t0)
        if tb <= ta then Nothing else Just outputT
  go (x bbox) V.x <|> go (y bbox) V.y <|> go (z bbox) V.z

getAabbComponent :: Aabb -> Axis -> Interval
getAabbComponent bbox axis = case axis of
  X -> x bbox
  Y -> y bbox
  Z -> z bbox

longestAxis :: Aabb -> Axis
longestAxis bbox = axis
  where
    cmp a b = compare (snd a) (snd b)
    axis = fst . maximumBy cmp $ zip [X, Y, Z] $ [x, y, z] <*> pure bbox

delta :: Double
delta = 0.0001

padToMinimum :: Interval -> Interval
padToMinimum interval = if intervalSize interval < delta then expandInterval interval delta else interval
