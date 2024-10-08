{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}

module Material (Material (..), SomeMaterial (..)) where

import HitRecord (HitRecord (..))
import Ray (Ray)
import System.Random.Stateful (StatefulGen)
import Vec3 (V3)

class Material object where
  scatterM ::
    (StatefulGen g m) =>
    object ->
    Ray ->
    HitRecord ->
    g ->
    m (Maybe (V3, Ray))

data SomeMaterial where
  MkSomeMaterial :: (Material material, Show material) => material -> SomeMaterial

instance Material SomeMaterial where
  scatterM (MkSomeMaterial material) = scatterM material

instance Show SomeMaterial where
  show (MkSomeMaterial mat) = show mat
