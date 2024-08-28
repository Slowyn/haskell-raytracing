module Optics (reflect, refract, shlick) where

import Vec3 (V3, Vec3 (..))

reflect :: V3 -> V3 -> V3
reflect v n = v <-> n .^ (v .* n) .^ 2

refract :: V3 -> V3 -> Double -> V3
refract uv n etaIOverEtaT = rOutPerp <+> rOutParallel
  where
    cosTheta = min (invert uv .* n) 1
    rOutPerp = (uv <+> n .^ cosTheta) .^ etaIOverEtaT
    rOutParallel = n .^ (-sqrt (abs (1 - squareNorm rOutPerp)))

shlick :: Double -> Double -> Double
shlick cosine refractionIndex = r0 + (1 - r0) * (1 - cosine) ** 5
  where
    r0 = ((1 - refractionIndex) / (1 + refractionIndex)) ** 2
