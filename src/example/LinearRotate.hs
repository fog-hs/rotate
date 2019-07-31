module LinearRotate where

import Linear.V3
import qualified Linear.Quaternion as Q

type Scalar = Double

type Vector = V3 Scalar

toVector :: Scalar -> Scalar -> Scalar -> Vector 
toVector = V3

rotate :: Vector -> Scalar -> Vector -> Vector	
rotate axis angle = Q.rotate (Q.axisAngle axis angle)