{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Camera (Camera (..), CameraTrait (..)) where

import Codec.Picture
import Hittable (HitRecord (..), Hittable (..))
import HittableList (HittableList)
import Ray (Ray, RayTrait (..))
import Vec3 (Vec3 (..))

-- Translate the [0,1] component values to the byte range [0,255]
vecToPixel :: forall v. (Vec3 v) => v -> PixelRGB8
vecToPixel v = PixelRGB8 r g b
  where
    (x, y, z) = toXYZ v
    r = floor $ 255.999 * x
    g = floor $ 255.999 * y
    b = floor $ 255.999 * z

rayColorBackground :: forall v. (Vec3 v) => Ray v -> PixelRGB8
rayColorBackground ray = vecToPixel color
  where
    (_origin, direction) = toVecs ray
    unitDirection = normalize direction
    a = 0.5 * (y unitDirection + 1.0)
    color :: v
    color = fromXYZ (1.0, 1.0, 1.0) .^ (1 - a) <+> fromXYZ (0.5, 0.7, 1.0) .^ a

-- | Hack to get infinity of Double
infinity :: Double
infinity = 1 / 0

rayColor :: forall v. (Vec3 v) => Ray v -> HittableList v -> PixelRGB8
rayColor ray world = case hit world ray 0 infinity of
  Just hitRecord -> vecToPixel $ (normal hitRecord <+> fromXYZ (1, 1, 1)) .^ 0.5
  Nothing -> rayColorBackground ray

calculateImageHeight :: Int -> Float -> Int
calculateImageHeight width aspectRatio = max 1 imageHeight
  where
    imageHeight = floor $ fromIntegral width / aspectRatio

class (Vec3 (CameraVecType c)) => CameraTrait c where
  type CameraVecType c
  createCamera :: Int -> Float -> c
  render :: c -> HittableList (CameraVecType c) -> Image PixelRGB8

data Camera v = (Vec3 v) => Camera
  { aspectRatio :: Float,
    width :: Int,
    height :: Int,
    center :: v,
    pixel00Loc :: v,
    pixelDeltaU :: v,
    pixelDeltaV :: v
  }

instance (Vec3 v) => CameraTrait (Camera v) where
  type CameraVecType (Camera v) = v
  createCamera :: Int -> Float -> Camera v
  createCamera width aspectRatio =
    Camera
      { aspectRatio,
        width,
        height,
        center,
        pixel00Loc,
        pixelDeltaU,
        pixelDeltaV
      }
    where
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

  render :: Camera v -> HittableList v -> Image PixelRGB8
  render camera world = generateImage renderPixel width height
    where
      Camera {width, height, center, pixel00Loc, pixelDeltaU, pixelDeltaV} = camera
      renderPixel x y = rayColor ray world
        where
          pixelCenter = pixel00Loc <+> (pixelDeltaU .^ fromIntegral x) <+> (pixelDeltaV .^ fromIntegral y)
          direction = pixelCenter <-> center
          ray = fromVecs center direction
