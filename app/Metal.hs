module Metal (Metal (..), mkMetal) where

import HitRecord (HitRecord (..))
import Material (Material (..))
import Random (uniformUnitVec3M)
import Ray (Ray, RayTrait (..))
import Vec3 (V3, Vec3 (..))

data Metal = Metal {albedo :: V3, fuzz :: Double}

instance Material Metal where
  scatter material rayIn hitRecord gen = do
    let reflected = reflect (getDirection rayIn) (normal hitRecord)
    fuzzVec <- (.^ fuzz material) <$> uniformUnitVec3M gen
    let fuzzReflected = reflected <+> fuzzVec
    let scattered = fromVecs (p hitRecord) fuzzReflected :: Ray
    let attenuation = albedo material
    return $ if getDirection scattered .* normal hitRecord > 0 then Just (attenuation, scattered) else Nothing

reflect :: V3 -> V3 -> V3
reflect v n = v <-> n .^ (v .* n) .^ 2

mkMetal :: V3 -> Double -> Metal
mkMetal albedo fuzz = Metal {albedo, fuzz}