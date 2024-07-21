{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Camera (createCamera, rayColor, GetPixelCenter, GetRay) where

import Codec.Picture
import Ray (Ray, RayTrait (..), fromCVecs)
import Vec3 (CVec3, Vec3 (..))

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

rayColor :: forall v. (Vec3 v) => Ray v -> PixelRGB8
rayColor ray =
  let sphereCenter = fromXYZ (0, 0, -1)
      t = hitSphere sphereCenter 0.5 ray
   in if t > 0
        then
          let n = normalize (at ray t <-> sphereCenter)
              colorVec = mapVec succ n .^ 0.5
           in vecToPixel colorVec
        else rayColorBackground ray

calculateImageHeight :: Int -> Float -> Int
calculateImageHeight width aspectRatio = max 1 imageHeight
  where
    imageHeight = floor $ fromIntegral width / aspectRatio

type GetPixelCenter = Int -> Int -> CVec3

type GetRay = CVec3 -> Ray CVec3

createCamera :: Int -> (Int, GetPixelCenter, GetRay)
createCamera imageWidth =
  let aspectRatio :: Float = 16.0 / 9.0
      imageHeight = calculateImageHeight imageWidth aspectRatio
      focalLength = 1.0
      viewportHeight = 2.0
      viewportWidth = viewportHeight * (fromIntegral imageWidth / fromIntegral imageHeight)
      cameraCenter = fromXYZ (0, 0, 0) :: CVec3
      viewportU = fromXYZ (viewportWidth, 0, 0) :: CVec3
      viewportV = fromXYZ (0, -viewportHeight, 0) :: CVec3
      pixelDeltaU = viewportU /^ fromIntegral imageWidth
      pixelDeltaV = viewportV /^ fromIntegral imageHeight
      viewportUpperLeft = cameraCenter <-> fromXYZ (0, 0, focalLength) <-> viewportU /^ 2 <-> viewportV /^ 2
      pixel00Loc = viewportUpperLeft <+> (pixelDeltaU <+> pixelDeltaV) .^ 0.5
      getPixelCenter :: GetPixelCenter
      getPixelCenter x y = pixel00Loc <+> (pixelDeltaU .^ fromIntegral x) <+> (pixelDeltaV .^ fromIntegral y)
      getRay :: GetRay
      getRay pixelCenter = fromCVecs cameraCenter (pixelCenter <-> cameraCenter)
   in (imageHeight, getPixelCenter, getRay)

hitSphere :: (Vec3 v) => v -> Double -> Ray v -> Double
hitSphere center radius ray = if discriminant < 0 then -1.0 else (-b - sqrt discriminant) / (2 * a)
  where
    (origin, direction) = toVecs ray
    oc = center <-> origin
    a = direction .* direction
    b = -2.0 * (direction .* oc)
    c = oc .* oc - radius * radius
    discriminant = b * b - 4 * a * c