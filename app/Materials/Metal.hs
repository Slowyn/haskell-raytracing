module Materials.Metal (Metal (..), mkMetal) where

import HitRecord (HitRecord (..))
import Material (Material (..))
import Optics (reflect)
import Random (uniformUnitVec3M)
import Ray (Ray (..), RayTrait (..), mkRay)
import Vec3 (V3, Vec3 (..))

data Metal = Metal {albedo :: V3, fuzz :: Double} deriving (Show)

instance Material Metal where
  scatterM material rayIn hitRecord gen = do
    let reflected = reflect (getDirection rayIn) (normal hitRecord)
    fuzzVec <- (.^ fuzz material) <$> uniformUnitVec3M gen
    let fuzzReflected = reflected <+> fuzzVec
    let scattered = mkRay (p hitRecord) fuzzReflected (time rayIn)
    let attenuation = albedo material
    return $ if getDirection scattered .* normal hitRecord > 0 then Just (attenuation, scattered) else Nothing

mkMetal :: V3 -> Double -> Metal
mkMetal albedo fuzz = Metal {albedo, fuzz}
