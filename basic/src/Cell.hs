-- Cell.hs
-- T. M. Kelley
-- Feb 09, 2011
-- (c) Copyright 2011 LANSLLC, all rights reserved

module Cell where

import Physical

data BoundaryCondition = Vac | Refl | Transp | None deriving (Show)

data BoundaryConditions = BCs { xbc :: BoundaryCondition
                              , ybc :: BoundaryCondition
                              , zbc :: BoundaryCondition} deriving (Show)

-- hope this works in 1D and 3D
data CellProperties = CellProps {
      low_b   :: Position -- claiming we cd rep a rectangular cell with two vertices
    , high_b  :: Position
    , low_bc  :: BoundaryConditions
    , high_bc :: BoundaryConditions
} deriving (Show)


data Face = XLow | XHigh | YLow | YHigh | ZLow | ZHigh deriving (Show,Eq)

-- | convenience function: set up 1D B.C. without typing all the None's 
bc1D :: BoundaryCondition -> BoundaryConditions
bc1D bc = (BCs bc None None) 


-- version
-- $Id$

-- End of file
