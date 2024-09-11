module HitRecord (HitRecord (..), mkHitRecord, solveFrontFaceNorm) where

import Ray
import Vec3

data HitRecord = HitRecord
  { p :: !V3,
    normal :: !V3,
    t :: !Double,
    frontFace :: !Bool,
    u :: !Double,
    v :: !Double
  }
  deriving (Show, Eq)

mkHitRecord :: V3 -> V3 -> Double -> Bool -> Double -> Double -> HitRecord
mkHitRecord p normal t frontFace u v =
  HitRecord
    { p,
      normal,
      t,
      frontFace,
      u,
      v
    }

solveFrontFaceNorm :: Ray -> V3 -> (Bool, V3)
solveFrontFaceNorm ray outwardNorm = if frontFace then (frontFace, outwardNorm) else (frontFace, invert outwardNorm)
  where
    frontFace = getDirection ray .* outwardNorm < 0
