{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Camera (Camera, CameraTrait (..))
import Codec.Picture
import Data.Time.Clock
import HittableList (AnyHittable (AnyHittable), HittableList (..))
import Sphere (Sphere (..))
import System.Random (mkStdGen)
import Text.Printf
import Vec3 (CVec3, Vec3 (fromXYZ))

width :: Int
width = 512

main :: IO ()
main = do
  t1 <- getCurrentTime
  printf "Render started at %s\n" (show t1)
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
  t2 <- getCurrentTime
  printf "Render finished at %s\n" (show t2)
  printf "Time taken: %s\n" (show $ diffUTCTime t2 t1)
  t3 <- getCurrentTime
  printf "Start saving image to jpg file at %s\n" (show $ diffUTCTime t2 t1)
  saveJpgImage 100 "test.jpg" image
  t4 <- getCurrentTime
  printf "saving image to jpg file finished in %s\n" (show $ diffUTCTime t4 t3)
  printf "Time taken: %s\n" (show $ diffUTCTime t2 t1)
  printf "Program finished in %s\n" (show $ diffUTCTime t4 t1)
