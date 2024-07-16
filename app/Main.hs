module Main where

import GHC.Base
import Vec3

data P3 = P3
  { width :: Int,
    height :: Int,
    imageData :: List (Int, Int, Int)
  }

getP3Row :: Int -> Int -> Int -> Int -> String
getP3Row w h i j =
  let r :: Double = fromIntegral i / fromIntegral (w - 1)
      g :: Double = fromIntegral j / fromIntegral (h - 1)
      b :: Double = 0.0
      ir :: Int = floor $ 255.999 * r
      ig :: Int = floor $ 255.999 * g
      ib :: Int = floor $ 255.999 * b
   in (show ir ++ " " ++ show ig ++ " " ++ show ib ++ "\n")

renderP3 :: P3 -> String
renderP3 p3 =
  let w = width p3
      h = height p3
      header = "P3\n" ++ show w ++ " " ++ show h ++ "\n255\n"
      getP3RowWH = getP3Row w h
      body = unwords [getP3RowWH i j | j <- [0 .. h - 1], i <- [0 .. w - 1]]
   in header ++ " " ++ body

main :: IO ()
main = do
  let p3 =
        P3
          { width = 256,
            height = 256,
            imageData = []
          }
      str = renderP3 p3
  let v1 = fromXYZ (15, 1, 2) :: TVec3
  print v1
  writeFile "image.ppm" str
  putStrLn "Finished"