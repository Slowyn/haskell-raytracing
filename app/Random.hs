{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Random
  ( uniformVec3M,
    uniformVec3ListM,
    uniformUnitVec3M,
    uniformVec3InUnitSphereM,
    uniformVec3OnHemiSphereM,
    uniformVec3OnUnitDiskM,
  )
where

import Control.Monad (replicateM)
import System.Random.Stateful
import Vec3 (V3, Vec3 (..))

uniformVec3M :: (StatefulGen g m) => (Double, Double) -> g -> m V3
uniformVec3M range gen = do
  x <- uniformRM range gen
  y <- uniformRM range gen
  z <- uniformRM range gen
  pure $ fromXYZ (x, y, z)

uniformVec3ListM :: (StatefulGen g m) => (Double, Double) -> Int -> g -> m [V3]
uniformVec3ListM range n = replicateM n . uniformVec3M range

uniformUnitVec3M :: (StatefulGen g m) => g -> m V3
uniformUnitVec3M gen = do
  randomUnitVector <- uniformVec3InUnitSphereM gen
  return $ normalize randomUnitVector

uniformVec3OnHemiSphereM :: (StatefulGen g m) => V3 -> g -> m V3
uniformVec3OnHemiSphereM normal gen = do
  onUnitSphere <- uniformUnitVec3M gen
  if onUnitSphere .* normal > 0
    then return onUnitSphere
    else return $ invert onUnitSphere

uniformVec3InUnitSphereM :: (StatefulGen g m) => g -> m V3
uniformVec3InUnitSphereM gen = do
  r <- uniformRM (0, 1) gen
  theta <- uniformRM (0, pi * 2) gen
  phi <- uniformRM (0, pi * 2) gen
  pure $ fromXYZ (r * sin theta * cos phi, r * sin theta * sin phi, r * cos theta)

uniformVec3OnUnitDiskM :: (StatefulGen g m) => g -> m V3
uniformVec3OnUnitDiskM gen = do
  r <- uniformRM (0, 1) gen
  phi <- uniformRM (0, pi * 2) gen
  pure $ fromXYZ (r * cos phi, r * sin phi, 0)
