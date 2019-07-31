{-# Language MultiParamTypeClasses,TypeFamilies,UndecidableInstances #-}

module LinearRotate where

import Linear.V3
import qualified Linear.Quaternion as Q

import Rotate

testQRotate n = Q.rotate (Q.axisAngle (V3 1 0 0) n) (V3 0 0 1)

{-
*Main> testQRotate (pi/2)
V3 0.0 (-1.0) 2.220446049250313e-16
-}

type Scalar = Double

type Vector = V3 Scalar

toVector = V3

rotate axis angle = Q.rotate (Q.axisAngle axis angle)