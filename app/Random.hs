{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Random
  ( uniformVec3M,
    uniformVec3ListM,
    uniformUnitVec3M,
    uniformVec3InUnitSphereM,
    uniformVec3OnHemiSphereM,
  )
where

import Control.Monad (replicateM)
import System.Random.Stateful
import Vec3

uniformVec3M :: (StatefulGen g m, Vec3 v) => (Double, Double) -> g -> m v
uniformVec3M range gen = do
  x <- uniformRM range gen
  y <- uniformRM range gen
  z <- uniformRM range gen
  pure $ fromXYZ (x, y, z)

uniformVec3ListM :: (StatefulGen g m, Vec3 v) => (Double, Double) -> Int -> g -> m [v]
uniformVec3ListM range n = replicateM n . uniformVec3M range

uniformVec3UntillM :: (StatefulGen g m, Vec3 v) => (Double, Double) -> (v -> Bool) -> g -> m v
uniformVec3UntillM range predicate gen = do
  vecCandidate <- uniformVec3M range gen
  if predicate vecCandidate
    then return vecCandidate
    else uniformVec3UntillM range predicate gen

uniformVec3InUnitSphereM :: (StatefulGen g m, Vec3 v) => g -> m v
uniformVec3InUnitSphereM = uniformVec3UntillM (-1, 1) isVec3InUnitSphere

isVec3InUnitSphere :: (Vec3 v) => v -> Bool
isVec3InUnitSphere = (< 1) . squareNorm

uniformUnitVec3M :: (StatefulGen g m, Vec3 v) => g -> m v
uniformUnitVec3M gen = do
  randomUnitVector <- uniformVec3InUnitSphereM gen
  return $ normalize randomUnitVector

uniformVec3OnHemiSphereM :: (StatefulGen g m, Vec3 v) => v -> g -> m v
uniformVec3OnHemiSphereM normal gen = do
  onUnitSphere <- uniformUnitVec3M gen
  if onUnitSphere .* normal > 0
    then return onUnitSphere
    else return $ invert onUnitSphere
