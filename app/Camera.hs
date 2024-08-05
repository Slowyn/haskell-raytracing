{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Camera (Camera (..), CameraTrait (..)) where

import Codec.Picture
import Control.Monad (replicateM)
import Control.Monad.Primitive (PrimMonad)
import FoldHittable (FoldHittable (nearestHit))
import HittableList (HittableList)
import Material (Material (scatterM), SomeMaterial (MkSomeMaterial))
import Random (uniformVec3M, uniformVec3OnUnitDiskM)
import Ray (Ray (..), RayTrait (..))
import System.Random.Stateful (StatefulGen)
import Vec3 (V3, Vec3 (..))

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

linearToGamma :: Double -> Double
linearToGamma linearComponent = if linearComponent > 0 then sqrt linearComponent else 0

-- Translate the [0,1] component values to the byte range [0,255]
vecToPixel :: V3 -> PixelRGB8
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

rayColorBackground :: Ray -> V3
rayColorBackground ray = color
  where
    (_origin, direction) = toVecs ray
    unitDirection = normalize direction
    a = 0.5 * (y unitDirection + 1.0)
    color = fromXYZ (1.0, 1.0, 1.0) .^ (1 - a) <+> fromXYZ (0.5, 0.7, 1.0) .^ a

rayColorM :: (StatefulGen g m) => Ray -> HittableList -> Int -> g -> m V3
rayColorM ray world depth gen = case nearestHit world ray 0.001 infinity of
  Just (hitRecord, MkSomeMaterial material) -> do
    if depth <= 0
      then pure $ fromXYZ (0, 0, 0)
      else do
        scatterResult <- scatterM material ray hitRecord gen
        case scatterResult of
          Just (attenuation, scattered) -> do
            newColor <- rayColorM scattered world (depth - 1) gen
            pure $ attenuation <.> newColor
          Nothing -> pure $ fromXYZ (0, 0, 0)
  Nothing -> return $ rayColorBackground ray

calculateImageHeight :: Int -> Double -> Int
calculateImageHeight width aspectRatio = max 1 imageHeight
  where
    imageHeight = floor $ fromIntegral width / aspectRatio

class CameraTrait c where
  createCamera :: Int -> Double -> Int -> V3 -> V3 -> V3 -> Double -> Double -> Int -> Int -> c
  getRayM :: (StatefulGen g m) => c -> Int -> Int -> g -> m Ray
  renderPixelM :: (StatefulGen g m, PrimMonad m) => c -> Int -> Int -> HittableList -> g -> m PixelRGB8
  renderM :: (StatefulGen g m, PrimMonad m) => c -> HittableList -> g -> m (Image PixelRGB8)

data Camera = Camera
  { aspectRatio :: Double,
    width :: Int,
    height :: Int,
    samplesPerPixel :: Int,
    pixelSamplesScale :: Double,
    center :: V3,
    pixel00Loc :: V3,
    pixelDeltaU :: V3,
    pixelDeltaV :: V3,
    maxDepth :: Int,
    vfov :: Int,
    lookFrom :: V3,
    looktAt :: V3,
    vUp :: V3,
    defocusAngle :: Double,
    focusDist :: Double,
    defocusDiskU :: V3,
    defocusDiskV :: V3
  }

instance CameraTrait Camera where
  createCamera :: Int -> Double -> Int -> V3 -> V3 -> V3 -> Double -> Double -> Int -> Int -> Camera
  createCamera width aspectRatio vfov lookFrom looktAt vUp defocusAngle focusDist samplesPerPixel maxDepth =
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
        maxDepth,
        vfov,
        lookFrom,
        looktAt,
        vUp,
        defocusAngle,
        focusDist,
        defocusDiskU,
        defocusDiskV
      }
    where
      theta = degreeToRad (fromIntegral vfov)
      h = tan (theta / 2)
      pixelSamplesScale = 1.0 / fromIntegral samplesPerPixel
      height = calculateImageHeight width aspectRatio
      center = lookFrom
      w = normalize (lookFrom <-> looktAt)
      u = normalize (vUp >< w)
      v = w >< u
      viewportHeight = 2.0 * h * focusDist
      viewportWidth = viewportHeight * (fromIntegral width / fromIntegral height)
      viewportU = u .^ viewportWidth
      viewportV = invert v .^ viewportHeight
      pixelDeltaU = viewportU /^ fromIntegral width
      pixelDeltaV = viewportV /^ fromIntegral height
      viewportUpperLeft = center <-> w .^ focusDist <-> viewportU /^ 2 <-> viewportV /^ 2
      pixel00Loc = viewportUpperLeft <+> (pixelDeltaU <+> pixelDeltaV) .^ 0.5
      defocusRadius = focusDist * tan (degreeToRad $ defocusAngle / 2)
      defocusDiskU = u .^ defocusRadius
      defocusDiskV = v .^ defocusRadius

  getRayM camera x y gen = do
    offset <- uniformVec3M (-1, 1) gen
    p <- uniformVec3OnUnitDiskM gen
    let Camera {center, pixel00Loc, pixelDeltaU, pixelDeltaV, defocusDiskU, defocusDiskV, defocusAngle} = camera
        (x', y', _z') = toXYZ offset
        (x'', y'', _) = toXYZ p
        defocusDiskSample = center <+> (defocusDiskU .^ x'') <+> (defocusDiskV .^ y'')
        pixelSample = pixel00Loc <+> pixelDeltaU .^ (fromIntegral x + x') <+> pixelDeltaV .^ (fromIntegral y + y')
        origin = if defocusAngle <= 0 then center else defocusDiskSample
        direction = pixelSample <-> origin
    pure $ fromVecs origin direction

  renderPixelM camera x y world gen = do
    let Camera {samplesPerPixel, pixelSamplesScale, maxDepth} = camera
    rays <- replicateM samplesPerPixel (getRayM camera x y gen)
    colors <- mapM (\ray -> rayColorM ray world maxDepth gen) rays
    let averageColor = mconcat colors .^ pixelSamplesScale
    return $ vecToPixel averageColor

  renderM :: (StatefulGen g m, PrimMonad m) => Camera -> HittableList -> g -> m (Image PixelRGB8)
  renderM camera world gen = do
    let Camera {width, height} = camera
        renderPixel x y = renderPixelM camera x y world gen
    withImage width height renderPixel

degreeToRad :: Double -> Double
degreeToRad degrees = degrees * pi / 180