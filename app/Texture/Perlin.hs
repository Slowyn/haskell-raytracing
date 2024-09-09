module Texture.Perlin (NoiseTexture (..), mkNoiseTexture) where

import Control.Monad.Primitive (PrimMonad)
import Data.Bits (xor, (.&.))
import Data.Massiv.Array qualified as MA
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import System.Random.Stateful (StatefulGen, uniformRM)
import Texture (Texture (..))
import Vec3 (V3, Vec3 (..))

pointCount :: Int
pointCount = 256

data Perlin = Perlin
  { xPermutations :: !(V.Vector Int),
    yPermutations :: !(V.Vector Int),
    zPermutations :: !(V.Vector Int),
    values :: !(V.Vector Double)
  }
  deriving (Show)

mkPerlin :: (StatefulGen g m, PrimMonad m) => g -> m Perlin
mkPerlin gen = do
  values :: V.Vector Double <- V.generateM pointCount (\_ -> uniformRM (0, 1) gen)
  let indices1 :: V.Vector Int = V.enumFromN 0 pointCount
  let indices2 :: V.Vector Int = V.enumFromN 0 pointCount
  let indices3 :: V.Vector Int = V.enumFromN 0 pointCount
  xPermutations <- permute indices1 gen
  yPermutations <- permute indices2 gen
  zPermutations <- permute indices3 gen
  pure $
    Perlin
      { xPermutations,
        yPermutations,
        zPermutations,
        values
      }

permute :: (StatefulGen g m, PrimMonad m) => V.Vector Int -> g -> m (V.Vector Int)
permute vec gen = do
  mvec <- V.unsafeThaw vec
  let indices = V.reverse $ V.enumFromN 0 (VM.length mvec)
  V.forM_
    indices
    ( \i -> do
        target <- uniformRM (0, i) gen
        VM.swap mvec i target
    )
  V.unsafeFreeze mvec

trilinearInterpolation :: MA.Array MA.P MA.Ix3 Double -> Double -> Double -> Double -> Double
trilinearInterpolation c u v w =
  MA.ifoldlS
    ( \acc (i MA.:> j MA.:. k) cur -> do
        let di = fromIntegral i * u + (1 - fromIntegral i) * (1 - u)
            dj = fromIntegral j * v + (1 - fromIntegral j) * (1 - v)
            dk = fromIntegral k * w + (1 - fromIntegral k) * (1 - w)
        acc + (di * dj * dk * cur)
    )
    0.0
    c

noise :: Perlin -> V3 -> Double
noise perlin point = trilinearInterpolation c u' v' w'
  where
    u = x point - (fromIntegral . floor . x) point
    v = y point - (fromIntegral . floor . y) point
    w = z point - (fromIntegral . floor . z) point
    u' = u * u * (3 - 2 * u)
    v' = v * v * (3 - 2 * v)
    w' = w * w * (3 - 2 * w)
    i :: Int = (floor . x) point
    j :: Int = (floor . y) point
    k :: Int = (floor . z) point
    c :: MA.Array MA.P MA.Ix3 Double =
      MA.makeArray
        MA.Seq
        (MA.Sz3 2 2 2)
        ( \(di MA.:> dj MA.:. dk) -> do
            let dx = (i + di) .&. 255
                dy = (j + dj) .&. 255
                dz = (k + dk) .&. 255
            values perlin V.! (dx `xor` dy `xor` dz)
        )

newtype NoiseTexture = NoiseTexture Perlin

instance Show NoiseTexture where
  show _ = "Noise Texture"

instance Texture NoiseTexture where
  value (NoiseTexture perlin) _u _v point = fromXYZ (1, 1, 1) .^ noise perlin point

mkNoiseTexture :: (StatefulGen g m, PrimMonad m) => g -> m NoiseTexture
mkNoiseTexture gen = do
  perlin <- mkPerlin gen
  pure $ NoiseTexture perlin
