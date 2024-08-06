module Aabb
  ( Aabb (..),
    mkAabb,
    mkAabbIntervals,
    mkAabbPoints,
    hitBox,
  )
where

import Control.Applicative
import Interval (Interval (..), defaultInterval, mkInterval)
import Ray (Ray, RayTrait (..))
import Vec3 qualified as V

data Aabb = Aabb {x :: !Interval, y :: !Interval, z :: !Interval}

defaultAabb :: Aabb
defaultAabb = Aabb {x = defaultInterval, y = defaultInterval, z = defaultInterval}

mkAabb :: Aabb
mkAabb = defaultAabb

mkAabbIntervals :: Interval -> Interval -> Interval -> Aabb
mkAabbIntervals xInterval yInterval zInterval =
  Aabb
    { x = xInterval,
      y = yInterval,
      z = zInterval
    }

mkAabbPoints :: V.V3 -> V.V3 -> Aabb
mkAabbPoints a b = mkAabbIntervals xInterval yInterval zInterval
  where
    xInterval = if V.x a <= V.x b then mkInterval (V.x a) (V.x b) else mkInterval (V.x b) (V.x a)
    yInterval = if V.y a <= V.y b then mkInterval (V.y a) (V.y b) else mkInterval (V.y b) (V.y a)
    zInterval = if V.z a <= V.z b then mkInterval (V.z a) (V.z b) else mkInterval (V.z b) (V.z a)

hitBox :: Aabb -> Ray -> Interval -> Maybe Interval
hitBox bbox rayIn (Interval rayTa rayTb) = do
  let (origin, direction) = toVecs rayIn
  let go :: Interval -> (V.V3 -> Double) -> Maybe Interval
      go (Interval a b) comp = do
        let adinv = comp direction
            t0 = (a - comp origin) * adinv
            t1 = (b - comp origin) * adinv
            outputT@(Interval ta tb)
              | t0 < t1 = Interval (min rayTa t0) (max rayTb t1)
              | otherwise = Interval (min rayTa t1) (max rayTb t0)
        if tb <= ta then Nothing else Just outputT
  go (x bbox) V.x <|> go (y bbox) V.y <|> go (z bbox) V.z