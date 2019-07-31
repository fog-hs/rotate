module VectRotate where

import Data.Vect.Float.Base
import Data.Vect.Float.Util.Quaternion

newtype Vec = Vec {unVec :: (Double,Double,Double)} deriving (Eq,Show)

toVec3 :: Vec  -> Vec3 
toVec3 v = (\ (a,b,c)-> Vec3 (realToFrac a)(realToFrac b)(realToFrac c) ) $ unVec v

fromVec3 :: Vec3 -> Vec
fromVec3 (Vec3 a b c) = Vec ((realToFrac a),(realToFrac b),(realToFrac c))

--

type Scalar = Double

type Vector = Vec 

toVector a b c = Vec (a,b,c)

rotate :: Vec -> Double -> Vec -> Vec
rotate axis angle = fromVec3 . actU (rotU (toVec3 axis) (realToFrac angle)) . toVec3



