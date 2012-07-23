{-#  LANGUAGE TypeSynonymInstances, FlexibleInstances  #-}

module Test.Arbitraries where

import Test.QuickCheck
import Control.Applicative ( (<$>) )
import Physical
import Cell
import Material
import Sigma_HBFC
import Constants (pmg)
import Sphere1D
import Particle
import qualified PRNG
import Event
import Histogram
import Source
import qualified Philo2
import qualified TallyIM as T_IM

import qualified Data.HashMap.Strict as HMap
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Word (Word32)

-- QUESTION: how to use NonNegative here?

gt0, ge0 :: (Arbitrary a, Ord a, Num a) => Gen a
ge0 = suchThat arbitrary (>= 0)
gt0 = suchThat arbitrary (> 0)

-- Things naturally positive, or maybe 0
instance Arbitrary Energy where
  arbitrary = Energy <$> ge0

instance Arbitrary Momentum where 
  arbitrary = arbitrary >>= \x -> return $ Momentum x

instance Arbitrary EnergyWeight where
  arbitrary = EnergyWeight <$> ge0

instance Arbitrary Temperature where
  arbitrary = Temperature <$> ge0

instance Arbitrary CellIdx where
  arbitrary = CellIdx <$> ge0

instance Arbitrary Time where
  arbitrary = Time <$> ge0

-- | Neutrino cross sections are so small that we typically
-- only see an interaction for very high densities. So, we steer
-- the densities toward larger numbers. Changing this requires
-- changing properties in Collision_Test (maybe elsewhere).
instance Arbitrary Density where
  arbitrary = do
    x <- choose (1.0,16.0)
    return $ Density (10**x)

instance Arbitrary NDensity where
  arbitrary = do
    (Density x) <- arbitrary
    return $ NDensity (x / pmg)

instance Arbitrary Opacity where
  arbitrary = Opacity <$> ge0

instance Arbitrary Distance where
  arbitrary = Distance <$> ge0

instance Arbitrary Velocity where
  arbitrary = Velocity <$> choose (-c,c)

instance Arbitrary Direction where
  arbitrary = Direction <$> choose (-1.0,1.0)

-- this is specific to 1D spherical:
instance Arbitrary Position where
  arbitrary = Position <$> ge0

instance Arbitrary Particle where
  arbitrary = do
    psn <- arbitrary
    drn <- arbitrary
    tm  <- arbitrary
    nrg <- arbitrary
    wt  <- arbitrary
    cidx <- arbitrary
    return $ Particle psn drn tm nrg wt cidx

instance Arbitrary Material where
  arbitrary = do
    v <- arbitrary
    t <- arbitrary
    rn <- arbitrary
    re <- arbitrary
    rp <- arbitrary
    return $ Material v t rn re rp

instance Arbitrary Cell where
  arbitrary = do
    lb <- arbitrary
    deltaUB <- arbitrary
    lbc <- arbitrary
    ubc <- arbitrary
    mat <- arbitrary
    let ub = Position $ Physical.pos lb + (abs . Physical.pos $ deltaUB)
    return $ Cell lb ub lbc ubc mat

instance Arbitrary Face where
  arbitrary = elements [Hi,Lo]

instance Arbitrary EventCandidate where
  arbitrary = do
    n <- choose (0,2) :: Gen Int
    d <- arbitrary
    f <- arbitrary
    return $ case n of
               0 -> CollisionCand d
               1 -> BoundaryCand  d f
               _ -> TimeoutCand   d


instance Arbitrary CollType where
  arbitrary = do
    n <- choose (0,3) :: Gen Int
    return $ case n of
               0 -> NuclAbs
               1 -> NuclEl
               2 -> EMinusInel
               _ -> EPlusInel



-- Gives us an arbitrary Cell with an arbitrary Position in that cell
data PositionInCell = PiC Cell Position deriving Show

instance Arbitrary PositionInCell where
  arbitrary = do
    c <- arbitrary
    x <- choose (Physical.pos . lowB $ c, Physical.pos . highB $ c)
    return $ PiC c (Position x)

instance Arbitrary Sphere1D where
  arbitrary = Sphere1D <$> arbitrary

instance Arbitrary a => Arbitrary (V.Vector a) where
  arbitrary = do
    cells <- arbitrary
    return $ V.fromList cells

instance Arbitrary BoundaryCondition where
  arbitrary = oneof [return Vac, return Refl, return Transp]

instance Arbitrary Lepton where
  arbitrary = oneof [return nuE, return nuEBar, return nuX, return nuXBar]

instance Arbitrary URD where
  arbitrary = URD <$> choose (0.0,1.0)

-- type used for limiting values passed to the Source.rejector tests
newtype Alpha = Alpha {a :: FP} deriving Show
instance Arbitrary Alpha where
  arbitrary = Alpha <$> choose (0.0,10.0)

instance Arbitrary EHist where
  arbitrary = do
    -- bbounds: a monotonic list of energies with at least two entries
    energies <- arbitrary -- :: [Energy]
    e1 <- arbitrary
    e2 <- arbitrary
    let bbounds = Prelude.scanl1 (+) (e1:(e2:energies))
    return $ mkHist bbounds

-- arbitrary EHist, with an energy guaranteed to be in it
data HistNEnergy = HistNEnergy EHist Energy deriving Show

instance Arbitrary HistNEnergy where
  arbitrary = do
    h <- arbitrary
    let (Energy emin) = U.minimum (ehbins h) :: Energy
    let (Energy emax) = U.maximum (ehbins h) :: Energy
    e <- choose (emin,emax)
    return $ HistNEnergy h (Energy e)

instance Arbitrary Philo2.Philo4x32Ctr where
  arbitrary = do
    c1 <- arbitrary
    c2 <- arbitrary
    c3 <- arbitrary
    c4 <- arbitrary
    return $ Philo2.Ctr4x32  c1 c2 c3 c4

instance Arbitrary Philo2.Philo4x32Key where
  arbitrary = do
    k1 <- arbitrary
    k2 <- arbitrary
    return $ Philo2.Key4x32  k1 k2

instance Arbitrary Philo2.RNG where
  arbitrary = do
    ctr <- arbitrary
    key <- arbitrary
    return $ Philo2.RNG ctr key


-- Tally (from TallyIM)
-- Remember: there are more constraints among Tally elememts than expressed here
instance Arbitrary T_IM.EventCount where
  arbitrary = do
    nNA   <- ge0
    nNE   <- ge0
    nEMI  <- ge0
    nEPI  <- ge0
    nXMit <- ge0
    nRefl <- ge0
    nEsc  <- ge0
    nTO   <- ge0
    return $ T_IM.EventCount nNA nNE nEMI nEPI nXMit nRefl nEsc nTO

instance Arbitrary T_IM.CellTally where
  arbitrary = do
    m <- arbitrary
    e <- arbitrary
    return $ T_IM.CellTally m e

instance Arbitrary T_IM.PhysicsTally where
  arbitrary = do
    cidxs <- arbitrary :: Gen [Int]
    pts   <- arbitrary :: Gen [T_IM.CellTally]
    return $ HMap.fromList (zip cidxs pts)

instance Arbitrary T_IM.Tally where
  arbitrary = do
    ec  <- arbitrary
    dep <- arbitrary
    escs <- arbitrary
    pl  <- arbitrary
    return $ T_IM.Tally ec dep escs pl


-- Types for testing Partitioning
data RankNComm = RankNComm Word32 Word32 deriving Show
instance Arbitrary RankNComm where
  arbitrary = do
    comm <- choose (1,100)
    rank <- choose (0,(comm-1))
    return $ RankNComm rank comm

-- Rank, CommSz, cumulative number of particles, number of particles per cell
-- rank & commSz s.t. rank < commSz, wholly arbitrary cumulative n particles,
-- and limited nc
data RankNCommNNC = RankNCommNNC Word32 Word32 Word32 SrcStat deriving Show
instance Arbitrary RankNCommNNC where
  arbitrary = do
    (RankNComm r c) <- arbitrary 
    cumN <- arbitrary
    nc <- choose (0,50000)
    return $ RankNCommNNC r c cumN (0,nc,0,0)

-- work around existing Arbitrary instance (a,b,c,d) 
data SrcStatsV = SrcStatsV {unssV :: [SrcStat]} deriving Show
data SrcStatV  = SrcStatV {unsV :: SrcStat} deriving Show

instance Arbitrary SrcStatV where
  arbitrary = do 
    n <- choose (0,100) :: Gen Word32
    return $ SrcStatV (0,n,0,0)

instance Arbitrary SrcStatsV where
  arbitrary = do 
    ss <- resize 15 $ listOf1 arbitrary
    return $ SrcStatsV ss

-- for testing chunkBy: need a list of objects and list of lengths, 
-- such that the sum of lengths <= length of the list of objects.
data Chunkable = Chunkable [Int] [Double] deriving Show

instance Arbitrary Chunkable where
  arbitrary = do
    xs <- arbitrary
    lCands <- vectorOf (length xs) (choose (1,length xs))
    let ls = foldr go [] lCands
          where go :: Int -> [Int] -> [Int]
                go i ys = case (i + (sum ys)) < length xs of 
                            True -> i:ys
                            False -> ys
    return $ Chunkable ls xs

-- end of file

