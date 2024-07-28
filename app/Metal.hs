module Metal (Metal (..)) where

import HitRecord (HitRecord (..))
import Material (Material (..))
import Ray (Ray, RayTrait (..))
import Vec3 (V3, Vec3 (..))

newtype Metal = Metal {albedo :: V3}

instance Material Metal where
  scatter material rayIn hitRecord _gen = do
    let reflected = reflect (getDirection rayIn) (normal hitRecord)
        scattered = fromVecs (p hitRecord) reflected :: Ray
        attenuation = albedo material
    pure $ Just (attenuation, scattered)

reflect :: V3 -> V3 -> V3
reflect v n = v <-> n .^ (v .* n) .^ 2