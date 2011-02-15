-- SoftEquiv.hs
-- T. M. Kelley
-- Jan 28, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module SoftEquiv where

{- Compare numbers for equality within some tolerance. 
 - The basic case is, if the absolute value of the difference of the 
 - value and the reference divided by the value is less than the tolerance,
 - evaluate True. If the value is zero, evaluate whether the reference is
 - less than the tolerance. -}
softEquiv :: (Num a, Ord a) => a -> a -> a -> Bool
softEquiv val ref tol | val == 0       = abs(ref) < tol
                      -- | abs(val) < tol = abs(ref) < tol
                      | otherwise      = abs(val - ref) < tol * abs(val)


-- version
-- $Id$

-- End of file
