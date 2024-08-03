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
import Random (uniformVec3M)
import Sphere (mkSphere)
import System.Random (mkStdGen)
import System.Random.Stateful (StatefulGen, newIOGenM, uniformRM)
import Text.Printf
import Vec3 (Vec3 (..))

width :: Int
width = 512

main :: IO ()
main = do
  t1 <- getCurrentTime
  printf "Program started at %s\n" (show t1)
  let lookFrom = fromXYZ (-13, 2, 3)
      lookAt = fromXYZ (0, 0, 0)
      vUp = fromXYZ (0, 1, 0)
      aspectRatio = 16.0 / 9.0
      vfov = 20
      defocusAngle = 0.6
      focusDist = 10
      samplesPerPixel = 40
      maxDepth = 20
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
  world <- finalScene 6 gen
  printf "SamplesPerPixel: %s\nMaxDepth: %s\nImage Width: %s\n" (show samplesPerPixel) (show maxDepth) (show width)
  image <- renderM camera world gen
  saveJpgImage 100 "test.jpg" (ImageRGB8 image)
  t2 <- getCurrentTime
  printf "Time taken: %s\n" (show $ diffUTCTime t2 t1)
  printf "Program finished %s\n" (show t2)

mapPairs :: (StatefulGen g m) => g -> (Int, Int) -> m SomeHittable
mapPairs gen (a, b) = do
  chooseMatP :: Double <- uniformRM (0, 1) gen
  offsetX <- uniformRM (0, 1) gen
  offsetZ <- uniformRM (0, 1) gen
  albedo <- (\v -> v <.> v) <$> uniformVec3M (0, 1) gen
  metalColor <- uniformVec3M (0.5, 1) gen
  let center = fromXYZ (fromIntegral a + 0.9 * offsetX, 0.2, fromIntegral b + 0.9 * offsetZ)
  let sphere
        | chooseMatP < 0.8 = SomeHittable $ mkSphere (Lambertian albedo) center 0.2
        | chooseMatP < 0.95 = SomeHittable $ mkSphere (mkMetal metalColor 0.5) center 0.2
        | otherwise = SomeHittable $ mkSphere (Dielectric 1.5) center 0.2
  return sphere

finalScene :: (StatefulGen g m) => Int -> g -> m HittableList
finalScene n gen = do
  let materialGround = Lambertian $ fromXYZ (0.5, 0.5, 0.5)
      material1 = Dielectric 1.5
      sphere1 = mkSphere material1 (fromXYZ (0, 1, 0)) 1.0
      material2 = Lambertian $ fromXYZ (0.4, 0.2, 0.1)
      sphere2 = mkSphere material2 (fromXYZ (-4, 1, 0)) 1.0
      material3 = mkMetal (fromXYZ (0.7, 0.6, 0.5)) 0
      sphere3 = mkSphere material3 (fromXYZ (4, 1, 0)) 1.0
      actualMapFn = mapPairs gen
      halfRange = floor (fromIntegral n / 2)
      (leftN, rightN) = (-halfRange, halfRange)
  let pairs = [(a, b) | a <- [leftN .. rightN], b <- [leftN .. rightN]]
  spheres <- mapM actualMapFn pairs
  return $
    HittableList $
      [ SomeHittable $ mkSphere materialGround (fromXYZ (0, -1000, -1)) 1000,
        SomeHittable sphere1,
        SomeHittable sphere2,
        SomeHittable sphere3
      ]
        ++ spheres
