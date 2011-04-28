{- Testing LT's & co -}

module Test.Geometry_Test (tests) where

import Test.QuickCheck
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Geometry
import Test.Arbitraries ()
import SoftEquiv
import qualified Physical as P
import Debug.Trace

prop_CM2Lab_Lab2CM_id el ol v  = (softEquiv e'' e tol)&&(softEquiv o'' o tol)
  where (ecm,ocm)  = labToComoving el ol v
        (el2,ol2)  = comovingToLab ecm ocm v
        e   = P.e el
        e'' = P.e el2
        o   = P.dir ol
        o'' = P.dir ol2
        tol = 1e-12

prop_Lab2CM_CM2Lab_id ecm ocm v  = (softEquiv e'' e tol)&&(softEquiv o'' o tol)
  where (el,ol)     = comovingToLab ecm ocm v
        (ecm2,ocm2) = labToComoving el ol v
        e   = P.e ecm
        e'' = P.e ecm2
        o   = P.dir ocm
        o'' = P.dir ocm2
        tol = 1e-12

tests = [testGroup "Lorentz Transforms" [
           testProperty  "comovingToLab inverse of labToComoving" prop_CM2Lab_Lab2CM_id
         , testProperty  "labToComoving inverse of comovingToLab" prop_Lab2CM_CM2Lab_id
                     ] 
        ]

