module Textures.Checker (CheckerTexture (..), mkCheckerTexture) where

import Texture (Texture (..))
import Textures.SolidColor (SolidColor)
import Vec3

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
