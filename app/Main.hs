{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Codec.Picture
import Ray (Ray, RayTrait (..))
import Vec3 (CVec3, Vec3 (..))

width :: Int
width = 256

height :: Int
height = 256

getPixelColor :: Int -> Int -> Int -> Int -> PixelRGB8
getPixelColor width height x y = PixelRGB8 ir ig ib
  where
    r :: Double = fromIntegral x / fromIntegral (width - 1)
    g :: Double = fromIntegral y / fromIntegral (height - 1)
    b :: Double = 0.0
    ir :: Pixel8 = floor $ 255.999 * r
    ig :: Pixel8 = floor $ 255.999 * g
    ib :: Pixel8 = floor $ 255.999 * b

imageCreate :: Int -> Int -> Image PixelRGB8
imageCreate width height = generateImage renderPixel width height
  where
    renderPixel = getPixelColor width height

main :: IO ()
main = do
  let image = ImageRGB8 $ imageCreate width height
      vec1 :: CVec3
      vec1 = fromXYZ (1, 2, 3)
      vec2 :: CVec3
      vec2 :: CVec3 = fromXYZ (3, 4, 5)
      exampleRay :: Ray CVec3 = fromVecs vec1 vec2
  saveJpgImage 100 "test.jpg" image
  print exampleRay
  putStrLn "Finished"