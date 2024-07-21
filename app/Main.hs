{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Main where

import Camera (GetPixelCenter, GetRay, createCamera, rayColor)
import Codec.Picture

width :: Int
width = 512

imageCreate :: Int -> Int -> GetPixelCenter -> GetRay -> Image PixelRGB8
imageCreate width height getPixelCenter getRay = generateImage renderPixel width height
  where
    renderPixel x y = rayColor ray
      where
        pixelCenter = getPixelCenter x y
        ray = getRay pixelCenter

main :: IO ()
main = do
  let (imageHeight, getPixelCenter, getRay) = createCamera width
      image = ImageRGB8 $ imageCreate width imageHeight getPixelCenter getRay

  saveJpgImage 100 "test.jpg" image
  putStrLn "Finished"
