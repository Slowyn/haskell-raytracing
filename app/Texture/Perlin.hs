module Texture.Perlin (NoiseTexture (..), mkNoiseTexture) where

import Control.Monad.Primitive (PrimMonad)
import Data.Bits (xor, (.&.))
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as VM
import System.Random.Stateful (StatefulGen, uniformRM)
import Texture (Texture (..))
import Vec3 (V3, Vec3 (..))

pointCount :: Int
pointCount = 256

data Perlin = Perlin
  { xPermutations :: V.Vector Int,
    yPermutations :: V.Vector Int,
    zPermutations :: V.Vector Int,
    values :: V.Vector Double
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

noise :: Perlin -> V3 -> Double
noise perlin point = values perlin V.! idx
  where
    i :: Int = xPermutations perlin V.! (floor (4 * x point) .&. 255)
    j :: Int = yPermutations perlin V.! (floor (4 * y point) .&. 255)
    k :: Int = zPermutations perlin V.! (floor (4 * z point) .&. 255)
    idx :: Int = xor k $ xor i j

newtype NoiseTexture = NoiseTexture Perlin

instance Show NoiseTexture where
  show _ = "Noise Texture"

instance Texture NoiseTexture where
  value (NoiseTexture perlin) _u _v point = fromXYZ (1, 1, 1) .^ noise perlin point

mkNoiseTexture :: (StatefulGen g m, PrimMonad m) => g -> m NoiseTexture
mkNoiseTexture gen = do
  perlin <- mkPerlin gen
  pure $ NoiseTexture perlin
