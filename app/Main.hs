{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Bvh (buildBvh)
import Camera (Camera, CameraTrait (..))
import Codec.Picture
import Data.Time.Clock
import Dielectric (Dielectric (..))
import HittableList (HittableList (..), mkHittableList)
import Lambertian (Lambertian (..))
import Metal (mkMetal)
import Object (SomeObject, mkSomeObject)
import Random (uniformVec3M)
import Sphere (mkMovingSphere, mkSphere)
import System.Random (mkStdGen)
import System.Random.Stateful (StatefulGen, newIOGenM, uniformRM)
import Text.Printf
import Texture (SolidColor (SolidColor), mkCheckerTexture)
import Vec3 (Vec3 (..))

width :: Int
width = 512

main :: IO ()
main = do
  t1 <- getCurrentTime
  printf "Program started at %s\n" (show t1)
  let lookFrom = fromXYZ (13, 2, 3)
      lookAt = fromXYZ (0, 0, 0)
      vUp = fromXYZ (0, 1, 0)
      aspectRatio = 16.0 / 9.0
      vfov = 20
      defocusAngle = 0.6
      focusDist = 10
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
  HittableList world <- finalScene 22 gen
  let bvhWorld = buildBvh world 0
  printf "SamplesPerPixel: %s\nMaxDepth: %s\nImage Width: %s\n" (show samplesPerPixel) (show maxDepth) (show width)
  image <- renderM camera bvhWorld gen
  saveJpgImage 100 "test.jpg" (ImageRGB8 image)
  t2 <- getCurrentTime
  printf "Time taken: %s\n" (show $ diffUTCTime t2 t1)
  printf "Program finished %s\n" (show t2)

mapPairs :: (StatefulGen g m) => g -> (Int, Int) -> m SomeObject
mapPairs gen (a, b) = do
  chooseMatP :: Double <- uniformRM (0, 1) gen
  offsetX <- uniformRM (0, 1) gen
  offsetZ <- uniformRM (0, 1) gen
  albedo <- SolidColor . (\v -> v <.> v) <$> uniformVec3M (0, 1) gen
  metalColor <- uniformVec3M (0.5, 1) gen
  let center = fromXYZ (fromIntegral a + 0.9 * offsetX, 0.2, fromIntegral b + 0.9 * offsetZ)
  center2 <- (<+> center) . fromXYZ . (0,,0) <$> uniformRM (0, 1) gen
  let sphere
        | chooseMatP < 0.8 = mkSomeObject (mkMovingSphere center center2 0.2) (Lambertian albedo)
        | chooseMatP < 0.95 = mkSomeObject (mkSphere center 0.2) (mkMetal metalColor 0.5)
        | otherwise = mkSomeObject (mkSphere center 0.2) (Dielectric 1.5)
  return sphere

finalScene :: (StatefulGen g m) => Int -> g -> m HittableList
finalScene n gen = do
  let checker =
        mkCheckerTexture
          (SolidColor $ fromXYZ (0.2, 0.3, 0.1))
          (SolidColor $ fromXYZ (0.9, 0.9, 0.9))
          0.32
      materialGround = Lambertian checker
      sphereGround = mkSphere (fromXYZ (0, -1000, -1)) 1000
      material1 = Dielectric 1.5
      sphere1 = mkSphere (fromXYZ (0, 1, 0)) 1.0
      material2 = Lambertian $ SolidColor (fromXYZ (0.4, 0.2, 0.1))
      sphere2 = mkSphere (fromXYZ (-4, 1, 0)) 1.0
      material3 = mkMetal (fromXYZ (0.7, 0.6, 0.5)) 0
      sphere3 = mkSphere (fromXYZ (4, 1, 0)) 1.0
      actualMapFn = mapPairs gen
      halfRange = floor (fromIntegral n / 2 :: Double)
      (leftN, rightN) = (-halfRange, halfRange)
  let pairs = [(a, b) | a <- [leftN .. rightN], b <- [leftN .. rightN]]
  spheres <- mapM actualMapFn pairs
  return $
    mkHittableList $
      [ mkSomeObject sphereGround materialGround,
        mkSomeObject sphere3 material3,
        mkSomeObject sphere1 material1,
        mkSomeObject sphere2 material2
      ]
        ++ spheres
