module HitRecord (HitRecord (..), createHitRecord, solveFrontFaceNorm) where

import Ray
import Vec3

data HitRecord = HitRecord
  { p :: !V3,
    normal :: !V3,
    t :: !Double,
    frontFace :: !Bool
  }
  deriving (Show, Eq)

createHitRecord :: V3 -> V3 -> Double -> Bool -> HitRecord
createHitRecord p normal t frontFace = HitRecord {p, normal, t, frontFace}

solveFrontFaceNorm :: Ray -> V3 -> (Bool, V3)
solveFrontFaceNorm ray outwardNorm = if frontFace then (frontFace, outwardNorm) else (frontFace, invert outwardNorm)
  where
    frontFace = getDirection ray .* outwardNorm < 0