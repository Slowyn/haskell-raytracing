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

type SomeObject = Object SomeHittable SomeMaterial

mkSomeObject :: (Hittable obj, Material a) => obj -> a -> Object SomeHittable SomeMaterial
mkSomeObject shape mat = Object (SomeHittable shape) (SomeMaterial mat)
