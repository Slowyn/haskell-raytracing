module Texture (Texture (..)) where

import Vec3 (V3, Vec3 (..))

class Texture t where
  value :: t -> Double -> Double -> V3 -> V3
  value _texture _u _v _point = origin
