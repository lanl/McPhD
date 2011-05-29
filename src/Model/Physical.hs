{-# LANGUAGE  GeneralizedNewtypeDeriving #-}

module Model.Physical where
{-| Types for various scalar physical quantities. Units given in comments.

From Tim's basic/Physical.hs Some derived instances of Vector types
removed. Need to ask what these were for.

Also, replaced the type synonym FP with Double. Not sure of the use
case for the synonym.

Removed Distance since this is provided via the Space type
family. Will it ever not be double?

Removed vector quantities Position, Direction, Momentum and Velocity,
since these are dependent on the Space type as well.

-}

-- | MeV
newtype Energy = Energy { e :: Double } deriving (Eq, Show, Num, Ord)
--  deriving (Eq, Show, Num, GV.Vector V.Vector, GMV.MVector V.MVector, Unbox)

-- | dimensionless
newtype EnergyWeight = EnergyWeight { ew :: Double } deriving (Eq, Show, Num, Ord)

-- | sec
newtype Time = Time { t :: Double } deriving (Eq, Show, Num, Ord)

-- | cm ^2
newtype CrossSection = CrossSection { sigma :: Double } deriving (Eq, Show, Num, Ord)

-- | cm ^ -1
newtype Opacity = Opacity { mu :: Double } deriving (Eq, Show, Num, Ord)

-- | MeV
newtype Temperature = Temperature { temp :: Double } deriving (Eq, Show, Num, Ord)

-- | g/(cc^3)
newtype Density = Density { rho :: Double } deriving (Eq, Show, Num, Ord)

-- | 1/(cc^3)
newtype NDensity = NDensity { nrho :: Double } deriving (Eq, Show, Num, Ord)

-- | dimensionless
newtype NucleonNumber = NucleonNumber { nnucl :: Double } deriving (Eq, Show, Num, Ord)

-- | MeV/sec
newtype Luminosity = Luminosity { lum :: Double } deriving (Eq,Show,Num,Ord)
