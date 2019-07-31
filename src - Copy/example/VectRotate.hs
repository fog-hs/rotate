{-# LANGUAGE UndecidableInstances,GeneralizedNewtypeDeriving, TypeFamilies,MultiParamTypeClasses #-} 

module VectRotate where

import Data.Vect.Float.Base
import Data.Vect.Float.Util.Quaternion

import Rotate

newtype Vec = Vec {unVec :: (Double,Double,Double)} deriving (Eq,Show)

toVec3 :: Vec  -> Vec3 
toVec3 v = (\ (a,b,c)-> Vec3 (realToFrac a)(realToFrac b)(realToFrac c) ) $ unVec v

fromVec3 :: Vec3 -> Vec
fromVec3 (Vec3 a b c) = Vec ((realToFrac a),(realToFrac b),(realToFrac c))

--

rotation :: (Vec,Double) -> Vec -> Vec
rotation (axis,angle) v = fromVec3 (actU (rotU (toVec3 axis) (realToFrac angle)) (toVec3 v))

testRotation n = rotation (Vec (1,0,0),n) (Vec (0,0,1))

{-
*Main> testRotation (pi/2)
Vec {unVec = (0.0,-0.9999999403953552,0.0)}
-}

type Scalar = Double

type Vector = Vec 

toVector a b c = Vec (a,b,c)

rotate axis angle = rotation (axis,angle)


