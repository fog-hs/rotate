module Main where

import qualified Vect.TestRotate as V
import qualified Linear.TestRotate as L

main :: IO ()
main = do
 (putStrLn . show) V.testRotate 
 (putStrLn . show) L.testRotate 

{-
to build and run this example, navigate to the directory containing rotate.cabal
type;
> cabal configure
...
>  cabal run rotate-example
...
Running rotate-example...
Vec {unVec = (0.0,-0.9999999403953552,0.0)}
V3 0.0 (-1.0) 2.220446049250313e-16
-}
