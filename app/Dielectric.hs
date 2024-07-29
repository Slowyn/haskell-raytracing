module Dielectric (Dielectric (..)) where

import HitRecord (HitRecord (..))
import Material (Material (..))
import Optics (refract)
import Ray (RayTrait (..))
import Vec3 (Vec3 (fromXYZ, normalize))

newtype Dielectric = Dielectric {refractionIndex :: Double}

instance Material Dielectric where
  scatterM material rayIn hitRecord _gen = do
    let attenuation = fromXYZ (1, 1, 1)
        ri = if frontFace hitRecord then 1 / refractionIndex material else refractionIndex material
        unitDirection = normalize (getDirection rayIn)
        refracted = refract unitDirection (normal hitRecord) ri
        scattered = fromVecs (p hitRecord) refracted
    return $ Just (attenuation, scattered)
