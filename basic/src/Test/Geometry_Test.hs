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


-- | LT with 0 velocity leaves inputs unchanged
prop_v0_id_CM2Lab e o = comovingToLab e o (P.Velocity 0.0) == (e,o)


-- | LT with 0 velocity leaves inputs unchanged
prop_v0_id_Lab2CM e o = labToComoving e o (P.Velocity 0.0) == (e,o)


-- | LT comoving->lab is inverse of LT lab->comoving
prop_CM2Lab_Lab2CM_id el ol v  = (softEquiv e'' e tol)&&(softEquiv o'' o tol)
    where (ecm,ocm)  = labToComoving el ol v
          (P.Energy e'',P.Direction o'')  = comovingToLab ecm ocm v
          e   = P.e el
          o   = P.dir ol
          tol = 1e-12


-- | LT lab->comoving is (soft) inverse of LT comoving->lab 
prop_Lab2CM_CM2Lab_id ecm ocm v  = (softEquiv e'' e tol)&&(softEquiv o'' o tol)
  where (el,ol)     = comovingToLab ecm ocm v
        (P.Energy e'',P.Direction o'') = labToComoving el ol v
        e   = P.e ecm
        o   = P.dir ocm
        tol = 1e-12

tests = [testGroup "Lorentz Transforms" 
         [
           testProperty "comovingToLab inverse of labToComoving" prop_CM2Lab_Lab2CM_id
         , testProperty "labToComoving inverse of comovingToLab" prop_Lab2CM_CM2Lab_id
         , testProperty "LT with 0 velocity is id (comoving -> lab)" prop_v0_id_CM2Lab
         , testProperty "LT with 0 velocity is id (lab -> comoving)" prop_v0_id_Lab2CM
         ] 
        ]

