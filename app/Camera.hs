{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Camera (Camera (..), CameraTrait (..)) where

import Codec.Picture
import Control.Monad.State
import Hittable (HitRecord (..), Hittable (..))
import HittableList (HittableList)
import Ray (Ray (..), RayTrait (..))
import System.Random (StdGen, uniformR)
import Vec3 (Vec3 (..))

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

-- Translate the [0,1] component values to the byte range [0,255]
vecToPixel :: forall v. (Vec3 v) => v -> PixelRGB8
vecToPixel v = PixelRGB8 r g b
  where
    (x, y, z) = toXYZ v
    clamp01 = clamp 0 1
    r = floor $ 255.999 * clamp01 x
    g = floor $ 255.999 * clamp01 y
    b = floor $ 255.999 * clamp01 z

rayColorBackground :: forall v. (Vec3 v) => Ray v -> v
rayColorBackground ray = color
  where
    (_origin, direction) = toVecs ray
    unitDirection = normalize direction
    a = 0.5 * (y unitDirection + 1.0)
    color :: v
    color = fromXYZ (1.0, 1.0, 1.0) .^ (1 - a) <+> fromXYZ (0.5, 0.7, 1.0) .^ a

-- | Hack to get infinity of Double
infinity :: Double
infinity = 1 / 0

rayColor :: forall v. (Vec3 v) => Ray v -> HittableList v -> v
rayColor ray world = case hit world ray 0 infinity of
  Just hitRecord -> (normal hitRecord <+> fromXYZ (1, 1, 1)) .^ 0.5
  Nothing -> rayColorBackground ray

calculateImageHeight :: Int -> Double -> Int
calculateImageHeight width aspectRatio = max 1 imageHeight
  where
    imageHeight = floor $ fromIntegral width / aspectRatio

class (Vec3 (CameraVecType c)) => CameraTrait c where
  type CameraVecType c
  createCamera :: Int -> Double -> Int -> c
  getRay :: c -> Int -> Int -> CameraVecType c -> Ray (CameraVecType c)
  render :: c -> HittableList (CameraVecType c) -> StdGen -> Image PixelRGB8

uniformVec3State :: (Vec3 v) => (Double, Double) -> StdGen -> (v, StdGen)
uniformVec3State range =
  runState $ do
    x <- state $ uniformR range
    y <- state $ uniformR range
    z <- state $ uniformR range
    pure $ fromXYZ (x, y, z)

data Camera v = (Vec3 v) => Camera
  { aspectRatio :: Double,
    width :: Int,
    height :: Int,
    samplesPerPixel :: Int,
    pixelSamplesScale :: Double,
    center :: v,
    pixel00Loc :: v,
    pixelDeltaU :: v,
    pixelDeltaV :: v
  }

instance (Vec3 v) => CameraTrait (Camera v) where
  type CameraVecType (Camera v) = v
  createCamera :: Int -> Double -> Int -> Camera v
  createCamera width aspectRatio samplesPerPixel =
    Camera
      { aspectRatio,
        width,
        height,
        samplesPerPixel,
        pixelSamplesScale,
        center,
        pixel00Loc,
        pixelDeltaU,
        pixelDeltaV
      }
    where
      pixelSamplesScale = 1.0 / fromIntegral samplesPerPixel
      height = calculateImageHeight width aspectRatio
      center = fromXYZ (0, 0, 0)
      focalLength = 1.0
      viewportHeight = 2.0
      viewportWidth = viewportHeight * (fromIntegral width / fromIntegral height)
      viewportU = fromXYZ (viewportWidth, 0, 0) :: v
      viewportV = fromXYZ (0, -viewportHeight, 0) :: v
      pixelDeltaU = viewportU /^ fromIntegral width
      pixelDeltaV = viewportV /^ fromIntegral height
      viewportUpperLeft = center <-> fromXYZ (0, 0, focalLength) <-> viewportU /^ 2 <-> viewportV /^ 2
      pixel00Loc = viewportUpperLeft <+> (pixelDeltaU <+> pixelDeltaV) .^ 0.5

  getRay :: Camera v -> Int -> Int -> v -> Ray v
  getRay camera x y offset = fromVecs origin direction
    where
      Camera {center, pixel00Loc, pixelDeltaU, pixelDeltaV} = camera
      (x', y', _z') = toXYZ offset
      pixelSample = pixel00Loc <+> pixelDeltaU .^ (fromIntegral x + x') <+> pixelDeltaV .^ (fromIntegral y + y')
      origin = center
      direction = pixelSample <-> origin

  render :: Camera v -> HittableList v -> StdGen -> Image PixelRGB8
  render camera world gen = generateImage renderPixel width height
    where
      Camera {width, height, samplesPerPixel, pixelSamplesScale} = camera
      renderPixel x y = vecToPixel color
        where
          accumulateOffsets :: ([v], StdGen) -> Int -> ([v], StdGen)
          accumulateOffsets (vectors, gen') _ = (vectors ++ [randomVec], gen'')
            where
              (randomVec, gen'') = uniformVec3State (-0.5, 0.5) gen'
          offsets = fst $ foldl accumulateOffsets ([], gen) [0 .. samplesPerPixel]
          color = foldl (<+>) (fromXYZ (0, 0, 0)) (map ((`rayColor` world) . getRay camera x y) offsets) .^ pixelSamplesScale
