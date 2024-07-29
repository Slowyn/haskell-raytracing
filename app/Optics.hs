module Optics (reflect, refract) where

import Vec3 (V3, Vec3 (..))

reflect :: V3 -> V3 -> V3
reflect v n = v <-> n .^ (v .* n) .^ 2

refract :: V3 -> V3 -> Double -> V3
refract uv n etaIOverEtaT = rOutPerp <+> rOutParallel
  where
    cosTheta = min (invert uv .* n) 1
    rOutPerp = (uv <+> n .^ cosTheta) .^ etaIOverEtaT
    rOutParallel = n .^ (-sqrt (abs (1 - squareNorm rOutPerp)))