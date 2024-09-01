module Texture (Texture (..), mkCheckerTexture, SolidColor (..), CheckerTexture (..)) where

import Vec3 (V3, Vec3 (..))

class Texture t where
  value :: t -> Double -> Double -> V3 -> V3
  value _texture _u _v _point = origin

newtype SolidColor = SolidColor V3 deriving (Show, Eq)

instance Texture SolidColor where
  value (SolidColor albedo) _ _ _ = albedo

data CheckerTexture = CheckerTexture !SolidColor !SolidColor !Double deriving (Show, Eq)

mkCheckerTexture :: SolidColor -> SolidColor -> Double -> CheckerTexture
mkCheckerTexture evenTexture oddTexture scale = CheckerTexture evenTexture oddTexture (1 / scale)

instance Texture CheckerTexture where
  value (CheckerTexture evenTexture oddTexture invScale) u v point =
    if isEven
      then value evenTexture u v point
      else value oddTexture u v point
    where
      xInteger :: Int = floor (invScale * x point)
      yInteger :: Int = floor (invScale * y point)
      zInteger :: Int = floor (invScale * z point)
      isEven = even (xInteger + yInteger + zInteger)