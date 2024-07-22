{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Camera (GetPixelCenter, GetRay, createCamera, rayColor)
import Codec.Picture
import HittableList (AnyHittable (AnyHittable), HittableList (..))
import Sphere (Sphere (..))
import Vec3 (CVec3, Vec3 (fromXYZ))

width :: Int
width = 512

imageCreate :: Int -> Int -> HittableList CVec3 -> GetPixelCenter -> GetRay -> Image PixelRGB8
imageCreate width height world getPixelCenter getRay = generateImage renderPixel width height
  where
    renderPixel x y = rayColor ray world
      where
        pixelCenter = getPixelCenter x y
        ray = getRay pixelCenter

main :: IO ()
main = do
  let (imageHeight, getPixelCenter, getRay) = createCamera width
      sphere1 :: Sphere CVec3
      sphere1 = Sphere (fromXYZ (0, 0, -1)) 0.5
      sphere2 :: Sphere CVec3
      sphere2 = Sphere (fromXYZ (0, -100.5, -1)) 100
      world :: HittableList CVec3
      world = HittableList [AnyHittable sphere1, AnyHittable sphere2]
      image = ImageRGB8 $ imageCreate width imageHeight world getPixelCenter getRay

  saveJpgImage 100 "test.jpg" image
  putStrLn "Finished"
