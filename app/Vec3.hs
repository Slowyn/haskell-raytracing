{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Vec3
  ( Vec3 (..),
    CVec3,
    uniformVec3State,
    uniformVec3List,
    uniformVec3InUnitSphere,
    uniformVec3OnHemiSphere,
  )
where

import Control.Monad (replicateM)
import Control.Monad.State
import System.Random
import System.Random.Stateful
import Prelude hiding (zipWith)

class Vec3 v where
  -- | Origin point @(0, 0, 0)@.
  origin :: v
  origin = fromXYZ (0, 0, 0)
  {-# INLINE origin #-}

  fromXYZ :: (Double, Double, Double) -> v
  toXYZ :: v -> (Double, Double, Double)

  x :: v -> Double
  x v = x where (x, _, _) = toXYZ v
  y :: v -> Double
  y v = y where (_, y, _) = toXYZ v
  z :: v -> Double
  z v = z where (_, _, z) = toXYZ v

  -- | Zip two vectors elementwise.
  zipWith :: (Double -> Double -> Double) -> v -> v -> v
  zipWith f v1 v2 = fromXYZ (f x1 x2, f y1 y2, f z1 z2)
    where
      (x1, y1, z1) = toXYZ v1
      (x2, y2, z2) = toXYZ v2

  -- | Create new vec with function
  mapVec :: (Double -> Double) -> v -> v
  mapVec f v1 = fromXYZ (f x, f y, f z)
    where
      (x, y, z) = toXYZ v1

  -- | Add two vectors.
  (<+>) :: v -> v -> v
  (<+>) = zipWith (+)
  {-# INLINE (<+>) #-}

  infixl 7 <+>

  -- | Subtract two vectors.
  (<->) :: v -> v -> v
  (<->) = zipWith (-)
  {-# INLINE (<->) #-}

  infixl 7 <->

  -- | Cross product.
  (><) :: v -> v -> v
  (><) v1 v2 =
    fromXYZ
      ( y1 * z2 - y2 * z1,
        x2 * z1 - x1 * z2,
        x1 * y2 - x2 * y1
      )
    where
      (x1, y1, z1) = toXYZ v1
      (x2, y2, z2) = toXYZ v2

  infixl 8 ><

  -- | Scale a vector.
  (.^) :: v -> Double -> v
  (.^) v s = fromXYZ (x * s, y * s, z * s)
    where
      (x, y, z) = toXYZ v

  infixl 9 .^

  -- | Divide a vector.
  (/^) :: v -> Double -> v
  (/^) v s = v .^ (1 / s)

  infixl 9 /^

  -- | Dot product.
  (.*) :: v -> v -> Double
  (.*) v1 v2 = x + y + z
    where
      (x, y, z) = toXYZ $ zipWith (*) v1 v2

  infixl 8 .*

  -- | Euclidean square norm of a vector.
  squareNorm :: v -> Double
  squareNorm v = v .* v
  {-# INLINE squareNorm #-}

  -- | Euclidean norm of a vector.
  norm :: v -> Double
  norm v = sqrt $ squareNorm v
  {-# INLINE norm #-}

  -- | Produce unit vector with the same direction as the original one.
  normalize :: v -> v
  normalize v = v .^ (1 / norm v)
  {-# INLINE normalize #-}

  -- | Distance between two points.
  distance :: v -> v -> Double
  distance v1 v2 = norm (v1 <-> v2)
  {-# INLINE distance #-}

  -- | Invert the direction of a vector.
  invert :: v -> v
  invert v = origin <-> v
  {-# INLINE invert #-}

data CVec3 = CVec3 !Double !Double !Double
  deriving (Eq)

instance Vec3 CVec3 where
  fromXYZ :: (Double, Double, Double) -> CVec3
  fromXYZ (x, y, z) = CVec3 x y z
  toXYZ (CVec3 x y z) = (x, y, z)

instance Show CVec3 where
  show v = "Vec3(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"
    where
      (x, y, z) = toXYZ v

uniformVec3M :: (StatefulGen g m, Vec3 v) => (Double, Double) -> g -> m v
uniformVec3M range gen = do
  x <- uniformRM range gen
  y <- uniformRM range gen
  z <- uniformRM range gen
  pure $ fromXYZ (x, y, z)

uniformVec3State :: (Vec3 v, RandomGen g) => (Double, Double) -> g -> (v, g)
uniformVec3State range =
  runState $ do
    x <- state $ uniformR range
    y <- state $ uniformR range
    z <- state $ uniformR range
    pure $ fromXYZ (x, y, z)

uniformVec3List :: (Vec3 v, RandomGen g) => (Double, Double) -> Int -> g -> [v]
uniformVec3List range n gen = runStateGen_ gen (replicateM n . uniformVec3M range)

uniformVec3UntillM :: (StatefulGen g m, Vec3 v) => (Double, Double) -> (v -> Bool) -> g -> m v
uniformVec3UntillM range predicate gen = do
  vecCandidate <- uniformVec3M range gen
  if predicate vecCandidate
    then return vecCandidate
    else uniformVec3UntillM range predicate gen

uniformVec3InUnitSphere :: (Vec3 v) => StdGen -> (v, StdGen)
uniformVec3InUnitSphere gen = runStateGen gen (uniformVec3UntillM (-1, 1) isVec3InUnitSphere)

isVec3InUnitSphere :: (Vec3 v) => v -> Bool
isVec3InUnitSphere = (< 1) . squareNorm

uniformVec3OnHemiSphere :: (Vec3 v) => v -> StdGen -> (v, StdGen)
uniformVec3OnHemiSphere normal gen = if onUnitSphere .* normal > 0 then (onUnitSphere, g') else (invert onUnitSphere, g')
  where
    (onUnitSphere, g') = uniformVec3InUnitSphere gen
