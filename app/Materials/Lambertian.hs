module Materials.Lambertian (Lambertian (..)) where

import HitRecord (HitRecord (..))
import Material (Material (..))
import Random (uniformUnitVec3M)
import Ray
import Texture (Texture (..))
import Vec3 (Vec3 (..))

newtype Lambertian a = Lambertian {albedo :: a} deriving (Show)

instance (Texture a) => Material (Lambertian a) where
  scatterM material rayIn hitRecord gen = do
    randomUnitVector <- uniformUnitVec3M gen
    let scatterDirection = normal hitRecord <+> randomUnitVector
        adjustedDirection = if nearZero scatterDirection then normal hitRecord else scatterDirection
        scattered = mkRay (p hitRecord) adjustedDirection (time rayIn)
        attenuation = value (albedo material) (u hitRecord) (v hitRecord) (p hitRecord)
    pure $ Just (attenuation, scattered)
