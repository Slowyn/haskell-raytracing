module Object (Object (..), SomeObject, mkSomeObject) where

import GHC.Generics (Generic, Generic1)
import Hittable (Hittable (..), SomeHittable (..))
import Material (Material (..), SomeMaterial (..))

data Object shape material = Object
  { shape :: !shape,
    material :: !material
  }
  deriving (Show, Eq, Ord, Generic, Generic1)

instance (Hittable shape) => Hittable (Object shape material) where
  hit = hit . shape
  boundingBox = boundingBox . shape

instance (Material material) => Material (Object shape material) where
  scatterM = scatterM . material

type SomeObject = Object SomeHittable SomeMaterial

mkSomeObject :: (Hittable obj, Material a, Show obj, Show a) => obj -> a -> Object SomeHittable SomeMaterial
mkSomeObject shape mat = Object (MkSomeHittable shape) (MkSomeMaterial mat)
