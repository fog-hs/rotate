{-# Language TypeFamilies, MultiParamTypeClasses,FlexibleContexts #-}

module TestRotate where

import Rotate

testRotate :: Vector
testRotate = rotate (toVector 1 0 0) (pi/2) (toVector 0 0 1)




