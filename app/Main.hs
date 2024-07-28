{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Camera (Camera, CameraTrait (..))
import Codec.Picture
import Data.Time.Clock
import HittableList (HittableList (..), SomeHittable (SomeHittable))
import Lambertian (Lambertian (..))
import Metal (Metal (..))
import Sphere (mkSphere)
import System.Random (mkStdGen)
import System.Random.Stateful (newIOGenM)
import Text.Printf
import Vec3 (Vec3 (fromXYZ))

width :: Int
width = 512

main :: IO ()
main = do
  t1 <- getCurrentTime
  printf "Program started at %s\n" (show t1)
  let materialGround = Lambertian $ fromXYZ (0.8, 0.8, 0)
      materialCenter = Lambertian $ fromXYZ (0.1, 0.2, 0.5)
      materialLeft = Metal $ fromXYZ (0.8, 0.8, 0.8)
      materialRight = Metal $ fromXYZ (0.8, 0.6, 0.2)
      world :: HittableList
      world =
        HittableList
          [ SomeHittable $ mkSphere materialGround (fromXYZ (0, -100.5, -1)) 100,
            SomeHittable $ mkSphere materialCenter (fromXYZ (0, 0, -1.2)) 0.5,
            SomeHittable $ mkSphere materialLeft (fromXYZ (-1, 0, -1)) 0.5,
            SomeHittable $ mkSphere materialRight (fromXYZ (1, 0, -1)) 0.5
          ]
      camera :: Camera
      camera = createCamera width (16.0 / 9.0) 100 50
  gen <- newIOGenM (mkStdGen 2024)
  image <- renderM camera world gen
  saveJpgImage 100 "test.jpg" (ImageRGB8 image)
  t2 <- getCurrentTime
  printf "Time taken: %s\n" (show $ diffUTCTime t2 t1)
  printf "Program finished %s\n" (show t2)
