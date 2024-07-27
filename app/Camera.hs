{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Camera (Camera (..), CameraTrait (..)) where

import Codec.Picture
import Control.Monad.Primitive (PrimMonad)
import Hittable (HitRecord (..), Hittable (..))
import HittableList (HittableList)
import Random (uniformUnitVec3M, uniformVec3ListM)
import Ray (Ray (..), RayTrait (..))
import System.Random.Stateful (StatefulGen)
import Vec3 (Vec3 (..))

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

linearToGamma :: Double -> Double
linearToGamma linearComponent = if linearComponent > 0 then sqrt linearComponent else 0

-- Translate the [0,1] component values to the byte range [0,255]
vecToPixel :: forall v. (Vec3 v) => v -> PixelRGB8
vecToPixel v = PixelRGB8 r g b
  where
    (x, y, z) = toXYZ $ mapVec linearToGamma v
    clamp01 = clamp 0 0.999
    r = floor $ 256 * clamp01 x
    g = floor $ 256 * clamp01 y
    b = floor $ 256 * clamp01 z

-- | Hack to get infinity of Double
infinity :: Double
infinity = 1 / 0

rayColorBackground :: forall v. (Vec3 v) => Ray v -> v
rayColorBackground ray = color
  where
    (_origin, direction) = toVecs ray
    unitDirection = normalize direction
    a = 0.5 * (y unitDirection + 1.0)
    color :: v
    color = fromXYZ (1.0, 1.0, 1.0) .^ (1 - a) <+> fromXYZ (0.5, 0.7, 1.0) .^ a

rayColorM :: (StatefulGen g m, Vec3 v) => Ray v -> HittableList v -> Int -> g -> m v
rayColorM ray world depth gen = case hit world ray 0.001 infinity of
  Just hitRecord -> do
    if depth <= 0
      then return $ fromXYZ (0, 0, 0)
      else do
        randomUnitVector <- uniformUnitVec3M gen
        let direction = normal hitRecord <+> randomUnitVector
        let newRay = fromVecs (p hitRecord) direction
        newRayColor <- rayColorM newRay world (depth - 1) gen
        return $ newRayColor .^ 0.1
  Nothing -> return $ rayColorBackground ray

calculateImageHeight :: Int -> Double -> Int
calculateImageHeight width aspectRatio = max 1 imageHeight
  where
    imageHeight = floor $ fromIntegral width / aspectRatio

class (Vec3 (CameraVecType c)) => CameraTrait c where
  type CameraVecType c
  createCamera :: Int -> Double -> Int -> Int -> c
  getRay :: c -> Int -> Int -> CameraVecType c -> Ray (CameraVecType c)
  renderPixelM :: (StatefulGen g m, PrimMonad m) => c -> Int -> Int -> HittableList (CameraVecType c) -> g -> m PixelRGB8
  renderM :: (StatefulGen g m, PrimMonad m) => c -> HittableList (CameraVecType c) -> g -> m (Image PixelRGB8)

data Camera v = (Vec3 v) => Camera
  { aspectRatio :: Double,
    width :: Int,
    height :: Int,
    samplesPerPixel :: Int,
    pixelSamplesScale :: Double,
    center :: v,
    pixel00Loc :: v,
    pixelDeltaU :: v,
    pixelDeltaV :: v,
    maxDepth :: Int
  }

instance (Vec3 v) => CameraTrait (Camera v) where
  type CameraVecType (Camera v) = v
  createCamera :: Int -> Double -> Int -> Int -> Camera v
  createCamera width aspectRatio samplesPerPixel maxDepth =
    Camera
      { aspectRatio,
        width,
        height,
        samplesPerPixel,
        pixelSamplesScale,
        center,
        pixel00Loc,
        pixelDeltaU,
        pixelDeltaV,
        maxDepth
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

  renderPixelM :: (StatefulGen g m, PrimMonad m) => Camera v -> Int -> Int -> HittableList v -> g -> m PixelRGB8
  renderPixelM camera x y world gen = do
    let Camera {samplesPerPixel, pixelSamplesScale, maxDepth} = camera
    offsets <- uniformVec3ListM (-0.5, 0.5) samplesPerPixel gen
    let rays = map (getRay camera x y) offsets
    colors <- mapM (\ray -> rayColorM ray world maxDepth gen) rays
    let averageColor = foldl (<+>) (fromXYZ (0, 0, 0)) colors .^ pixelSamplesScale
    return $ vecToPixel averageColor

  renderM :: (StatefulGen g m, PrimMonad m) => Camera v -> HittableList v -> g -> m (Image PixelRGB8)
  renderM camera world gen = do
    let Camera {width, height} = camera
        renderPixel x y = renderPixelM camera x y world gen
    withImage width height renderPixel
