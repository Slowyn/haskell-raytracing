module Interval
  ( Interval (..),
    mkInterval,
    mkInterval',
    expandInterval,
    intervalSize,
    intervalContains,
    intervalSurrounds,
    intervalClamp,
    defaultInterval,
    emptyInterval,
  )
where

import Data.Ord (clamp)

data Interval = Interval Double Double deriving (Show, Eq)

instance Ord Interval where
  (<=) a b = intervalSize a <= intervalSize b

defaultInterval :: Interval
defaultInterval = Interval 0 0

emptyInterval :: Interval
emptyInterval = Interval (-(1 / 0)) (1 / 0)

mkInterval :: Double -> Double -> Interval
mkInterval = Interval

mkInterval' :: Interval -> Interval -> Interval
mkInterval' interval1 interval2 = Interval min1 max2
  where
    Interval aMin aMax = interval1
    Interval bMin bMax = interval2
    min1 = min aMin bMin
    max2 = max aMax bMax

intervalSize :: Interval -> Double
intervalSize (Interval a b) = b - a

expandInterval :: Interval -> Double -> Interval
expandInterval (Interval minV maxV) delta = Interval (minV - padding) (maxV + padding)
  where
    padding = delta / 2

intervalContains :: Interval -> Double -> Bool
intervalContains (Interval a b) val = a <= val && val <= b

intervalSurrounds :: Interval -> Double -> Bool
intervalSurrounds (Interval a b) val = a < val && val < b

intervalClamp :: Interval -> Double -> Double
intervalClamp (Interval a b) = clamp (a, b)
