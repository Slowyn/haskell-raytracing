module Texture.SolidColor where

import Texture (Texture (..))
import Vec3 (V3)

newtype SolidColor = SolidColor V3 deriving (Show, Eq)

instance Texture SolidColor where
  value (SolidColor albedo) _ _ _ = albedo
