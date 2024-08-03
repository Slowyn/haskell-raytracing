module Lambertian (Lambertian (..)) where

import HitRecord (HitRecord (..))
import Material (Material (..))
import Random (uniformUnitVec3M)
import Ray
import Vec3 (V3, Vec3 (..))

newtype Lambertian = Lambertian {albedo :: V3} deriving (Show)

instance Material Lambertian where
  scatterM material _rayIn hitRecord gen = do
    randomUnitVector <- uniformUnitVec3M gen
    let scatterDirection = normal hitRecord <+> randomUnitVector
        adjustedDirection = if nearZero scatterDirection then normal hitRecord else scatterDirection
        scattered = fromVecs (p hitRecord) adjustedDirection :: Ray
        attenuation = albedo material
    pure $ Just (attenuation, scattered)
