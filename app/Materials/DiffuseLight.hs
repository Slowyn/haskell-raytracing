module Materials.DiffuseLight (DiffuseLight (..)) where

import Material (Material (..))
import Texture (Texture (..))

newtype DiffuseLight a = DiffuseLight {texture :: a} deriving (Show)

instance (Texture a) => Material (DiffuseLight a) where
  emitted (DiffuseLight {texture}) = value texture
