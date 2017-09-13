{- | Histogramming neutrino escape events. -}

{-# LANGUAGE DeriveGeneric #-}

module Histogram
  (
   -- * Energy weights, binned by energy
   EHist(..)
   -- * Create
  , mkHist
   -- * Count escape events
  , count
   -- * Examine the data
  , getCounts
   -- * Merge two EHist instances
  , combine
   -- * Helpers
  , findBin
  , divIfne0
  )
  where

import Control.Monad.ST.Strict as S (ST)
import Control.DeepSeq (NFData)
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import GHC.Generics (Generic)

import Physical

type EVector     = VU.Vector Energy
type EWVector    = VU.Vector EnergyWeight
type IVector     = VU.Vector Int
type BinBounds   = EVector -- static way to enforce monotonicity?

-- Note: EHist is not a monoid, since combining two Histrograms requires
-- they have the same bin boundaries.
data EHist = EHist { ehcounts  :: EWVector     -- ^ accumulated weights
                   , ehsquares :: EWVector    -- ^ accumulated squares of wts
                   , ehnevents :: IVector     -- ^ number of events per bin
                   , ehbins    :: BinBounds
                   }
             deriving (Show,Eq,Generic)

instance NFData EHist

-- | make an empty histogram with the given energy bin boundaries
mkHist :: [Energy] -> EHist
mkHist es = EHist (rep 0) (rep 0) (rep 0) (toBins es)
  where n   = (length es) - 1
        rep :: (VU.Unbox a,Num a) => a -> VU.Vector a
        rep m = VU.replicate n m

-- | Add an escape event to the histogram, in foldr order.
-- In principle, this uses a super-quick, O(1) destructive update.
count :: (Energy,EnergyWeight) -> EHist -> EHist
count (e,ew) h@(EHist cs ss ns bins) =
  let bin = findBin e bins
  in h { ehcounts  = accToBin cs bin ew
       , ehsquares = accToBin ss bin (ew*ew)
       , ehnevents = accToBin ns bin 1
       }

-- | mean weights per bin, and rms std dev of counts
getCounts :: EHist -> (EWVector,EWVector)
getCounts (EHist cs ss is _) = (cs,rmss)
  where rmss  = rms ss cs is

accToBin :: (VU.Unbox a,Num a) => VU.Vector a -> Int -> a -> VU.Vector a
accToBin v i x = VU.modify (add i x) v

add :: (VUM.Unbox a,Num a) => Int -> a -> VUM.MVector s a -> ST s ()
add i x v = VUM.read v i >>= (\x0 -> VUM.write v i (x + x0))

-- | find i such that bb!i <= e < bb!(i+1) if such an i exists, otherwise
-- throw an exception.
findBin :: Energy -> BinBounds -> Int
findBin e bb =
  case VU.findIndex (>= e) bb of
    Just i  -> i - 1
    Nothing -> error $ "energy " ++ show e ++ " outside of bin bounds: "
               ++ showBounds bb

-- | elementwise std deviation. Input vectors are
--   cs: summed energy weight in each bin  (\sum_i c_i)
--   ss: summed square of energy weight in each bin (\sum_i c_i^2)
--   ins: number of observations in each bin
-- The standard deviation in each bin is
-- [1/(n_i-1)*(\sum_i (c_i)^2 - 1/n * (\sum_i c_i)^2)]^(1/2)
rms :: (Eq a, Floating a,VU.Unbox a) => VU.Vector a -> VU.Vector a -> IVector -> VU.Vector a
rms ss cs ins = VU.map sqrt vars
  where vars  = VU.zipWith3 (\x y z -> (x - y) `divIfne0` z) ss csqs nm1s
        csqs  = VU.zipWith3 (\x y z -> (x * y) `divIfne0` z) cs cs ns
        ns    = VU.map fromIntegral ins
        nm1s  = VU.map (\x->x - 1) ns

divIfne0 :: (Eq a, Fractional a) => a -> a -> a
divIfne0 x y = if y /= 0 then (x/y) else x

showBounds :: BinBounds -> String
showBounds bb = "min: "   ++ show (VU.head bb) ++
                ", max: " ++ show (VU.last bb)

toBins :: [Energy] -> BinBounds
toBins es =
  if monotonic es
  then if (length es > 1)
       then VU.fromList es
       else error "must provide at least two energy bins"
  else error "non-monotonic energy bin boundaries"

monotonic :: [Energy] -> Bool
monotonic [] = True
monotonic [_e] = True
monotonic (e0:e1:es) = e0 <= e1 && monotonic (e1:es)

-- | Merge two EHist instance iff the bin boundaries are the same
combine :: EHist -> EHist -> EHist
combine (EHist cs1 ss1 ns1 bbs1) (EHist cs2 ss2 ns2 bbs2) =
  if bbs1 == bbs2
  then EHist (cs1 `zs` cs2) (ss1 `zs` ss2) (ns1 `zs` ns2) bbs1
  else error "Histogram.merge: don't know how to combine histograms with different bin boundaries"
    where zs :: (Num a,VU.Unbox a) => VU.Vector a -> VU.Vector a -> VU.Vector a
          zs v w = VU.zipWith (+) v w

-- end of file

