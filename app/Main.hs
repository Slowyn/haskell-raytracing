{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Camera (Camera, CameraTrait (..))
import Codec.Picture
import Data.Time.Clock
import Dielectric (Dielectric (..))
import HittableList (HittableList (..), SomeHittable (SomeHittable))
import Lambertian (Lambertian (..))
import Metal (mkMetal)
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
      materialLeft = Dielectric 1.5
      materialBubble = Dielectric $ 1.0 / 1.5
      materialRight = mkMetal (fromXYZ (0.8, 0.6, 0.2)) 1.0
      lookFrom = fromXYZ (-2, 2, 1)
      lookAt = fromXYZ (0, 0, -1)
      vUp = fromXYZ (0, 1, 0)
      world =
        HittableList
          [ SomeHittable $ mkSphere materialGround (fromXYZ (0, -100.5, -1)) 100,
            SomeHittable $ mkSphere materialCenter (fromXYZ (0, 0, -1.2)) 0.5,
            SomeHittable $ mkSphere materialLeft (fromXYZ (-1, 0, -1)) 0.5,
            SomeHittable $ mkSphere materialBubble (fromXYZ (-1, 0, -1)) 0.4,
            SomeHittable $ mkSphere materialRight (fromXYZ (1, 0, -1)) 0.5
          ]
      aspectRatio = 16.0 / 9.0
      vfov = 20
      defocusAngle = 10.0
      focusDist = 3.4
      samplesPerPixel = 100
      maxDepth = 50
      camera :: Camera
      camera =
        createCamera
          width
          aspectRatio
          vfov
          lookFrom
          lookAt
          vUp
          defocusAngle
          focusDist
          samplesPerPixel
          maxDepth
  gen <- newIOGenM (mkStdGen 2024)
  image <- renderM camera world gen
  saveJpgImage 100 "test.jpg" (ImageRGB8 image)
  t2 <- getCurrentTime
  printf "Time taken: %s\n" (show $ diffUTCTime t2 t1)
  printf "Program finished %s\n" (show t2)
