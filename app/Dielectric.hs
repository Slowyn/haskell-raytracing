module Dielectric (Dielectric (..)) where

import HitRecord (HitRecord (..))
import Material (Material (..))
import Optics (reflect, refract, shlick)
import Ray (Ray (..), RayTrait (..), mkRay)
import System.Random.Stateful (UniformRange (uniformRM))
import Vec3 (Vec3 (..))

newtype Dielectric = Dielectric {refractionIndex :: Double} deriving (Show)

instance Material Dielectric where
  scatterM material rayIn hitRecord gen = do
    randomValue <- uniformRM (0.0, 1.0 :: Double) gen
    let attenuation = fromXYZ (1, 1, 1)
        ri = if frontFace hitRecord then 1 / refractionIndex material else refractionIndex material
        unitDirection = normalize (getDirection rayIn)
        cosTheta = min (invert unitDirection .* normal hitRecord) 1
        sinTheta = sqrt (1 - cosTheta * cosTheta)
        cannotRefract = ri * sinTheta > 1
        reflectanseP = shlick cosTheta ri
        direction =
          if cannotRefract || reflectanseP > randomValue
            then reflect unitDirection (normal hitRecord)
            else refract unitDirection (normal hitRecord) ri
        scattered = mkRay (p hitRecord) direction (time rayIn)
    return $ Just (attenuation, scattered)
