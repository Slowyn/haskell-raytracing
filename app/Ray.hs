module Ray (Ray) where

import Vec3 (Vec3)

-- P(t)=A+tb
-- Where P is a 3D position along a line in 3D.
-- A is the ray origin and
-- b is the ray direction.
-- The ray parameter t is a real number

class Ray r

-- data Ray v = (Vec3 v) => Ray
--   { origin :: v,
--     destination :: v
--   }
