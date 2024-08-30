{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Vec3
  ( Vec3 (..),
    V3,
  )
where

import Prelude hiding (zipWith)

epsilon :: Double
epsilon = 1e-8

class Vec3 v where
  -- | Origin point @(0, 0, 0)@.
  origin :: v
  origin = fromXYZ (0, 0, 0)
  {-# INLINE origin #-}

  fromXYZ :: (Double, Double, Double) -> v
  fromValue :: Double -> v
  fromValue v = fromXYZ (v, v, v)
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
  {-# INLINE zipWith #-}

  -- | Create new vec with function
  mapVec :: (Double -> Double) -> v -> v
  mapVec f v1 = fromXYZ (f x, f y, f z)
    where
      (x, y, z) = toXYZ v1
  {-# INLINE mapVec #-}

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

  -- | Multiply two vectors.
  (<.>) :: v -> v -> v
  (<.>) = zipWith (*)
  {-# INLINE (<.>) #-}

  infixl 9 <.>

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

  nearZero :: v -> Bool
  nearZero v = x < epsilon && y < epsilon && z < epsilon
    where
      (x, y, z) = toXYZ v
  {-# INLINE nearZero #-}

data V3 = V3 !Double !Double !Double
  deriving (Eq)

instance Vec3 V3 where
  fromXYZ :: (Double, Double, Double) -> V3
  fromXYZ (x, y, z) = V3 x y z
  toXYZ (V3 x y z) = (x, y, z)

instance Show V3 where
  show v = "Vec3(" ++ show x ++ ", " ++ show y ++ ", " ++ show z ++ ")"
    where
      (x, y, z) = toXYZ v

instance Semigroup V3 where
  (<>) = (<+>)

instance Monoid V3 where
  mempty = origin
