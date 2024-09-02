module Scene (Scene (..), mkScene, renderSceneIO, SomeWorld (..)) where

import Camera (Camera (..), CameraTrait (renderM))
import Codec.Picture (Image, PixelRGB8)
import FoldHittable (FoldHittable (..))
import System.Random.Stateful (StatefulGen)

data SomeWorld where
  MkSomeWorld :: (FoldHittable world) => world -> SomeWorld

instance FoldHittable SomeWorld where
  nearestHit (MkSomeWorld world) = nearestHit world
  combinedBoundingBox (MkSomeWorld world) = combinedBoundingBox world

data Scene = Scene
  { camera :: !Camera,
    world :: !SomeWorld
  }

mkScene :: Camera -> SomeWorld -> Scene
mkScene camera world =
  Scene
    { camera,
      world
    }

renderSceneIO :: (StatefulGen g IO) => Scene -> g -> IO (Image PixelRGB8)
renderSceneIO scene = renderM (camera scene) (world scene)
