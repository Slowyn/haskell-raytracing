module Texture.Image where

import Codec.Picture (Image (..), PixelRGB8 (..), convertRGB8, pixelAt, readImage)
import Data.Ord (clamp)
import Texture (Texture (..))
import Vec3 (Vec3 (fromXYZ))

newtype ImageTexture = ImageTexture (Image PixelRGB8)

instance Show ImageTexture where
  show (ImageTexture image) = "ImageTexture (" ++ show (imageWidth image) ++ "x" ++ show (imageHeight image) ++ ")"

mkImageTexture :: FilePath -> IO ImageTexture
mkImageTexture filePath = do
  imageResult <- readImage filePath
  case imageResult of
    Left err -> error err
    Right image -> pure $ ImageTexture (convertRGB8 image)

instance Texture ImageTexture where
  value (ImageTexture image) u v _point = do
    let width = imageWidth image
        height = imageHeight image
        u' = clamp (0, 1) u
        v' = 1 - clamp (0, 1) v -- Flip V to image coordinates
        i :: Int = floor $ u' * fromIntegral width
        j :: Int = floor $ v' * fromIntegral height
        colorScale :: Double = 1.0 / 255.0
        (PixelRGB8 r g b) = pixelAt image i j
    fromXYZ (fromIntegral r * colorScale, fromIntegral g * colorScale, fromIntegral b * colorScale)
