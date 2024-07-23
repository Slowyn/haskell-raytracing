{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Camera (Camera, CameraTrait (..))
import Codec.Picture
import HittableList (AnyHittable (AnyHittable), HittableList (..))
import Sphere (Sphere (..))
import System.Random (mkStdGen)
import Vec3 (CVec3, Vec3 (fromXYZ))

width :: Int
width = 512

main :: IO ()
main = do
  let sphere1 :: Sphere CVec3
      sphere1 = Sphere (fromXYZ (0, 0, -1)) 0.5
      sphere2 :: Sphere CVec3
      sphere2 = Sphere (fromXYZ (0, -100.5, -1)) 100
      world :: HittableList CVec3
      world = HittableList [AnyHittable sphere1, AnyHittable sphere2]
      camera :: Camera CVec3
      camera = createCamera width (16.0 / 9.0) 50
      gen = mkStdGen 2024
      image = ImageRGB8 $ render camera world gen

  saveJpgImage 100 "test.jpg" image
  putStrLn "Finished"
