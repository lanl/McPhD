{-| Tests of opacity functions. -}

module Test.Opacity_Test where

import Physical
import Opacity
import Cell
import Material
import Test.Arbitraries

import Test.QuickCheck
import Test.Framework (testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

-- if corresponding density is zero, opacity is zero. So take 
-- an arbitrary Cell and force rhoNucl to 0. Since all the fields in
-- Material are strict, we need to provide them explicitly.

prop_rhoN0Total e c@(Cell {mat = m}) = opN c' e == Opacity 0.0
  where c' = c { mat = Material{sig_abs = sig_abs m
                                ,sig_scat = sig_scat m
                                ,Material.vel = Material.vel m
                                ,tempE = tempE m
                                ,rhoNucl = Density 0.0 
                                ,rhoEMinus = rhoEMinus m
                                ,rhoEPlus = rhoEPlus m}}

prop_rhoN0Abs e c@(Cell {mat = m}) = opNAbs c' e == Opacity 0.0
  where c' = c { mat = Material{sig_abs = sig_abs m
                                ,sig_scat = sig_scat m
                                ,Material.vel = Material.vel m
                                ,tempE = tempE m
                                ,rhoNucl = Density 0.0 
                                ,rhoEMinus = rhoEMinus m
                                ,rhoEPlus = rhoEPlus m}}

prop_rhoN0Elastic e c@(Cell {mat = m}) = opNElastic c' e == Opacity 0.0
  where c' = c { mat = Material{sig_abs = sig_abs m
                                ,sig_scat = sig_scat m
                                ,Material.vel = Material.vel m
                                ,tempE = tempE m
                                ,rhoNucl = Density 0.0 
                                ,rhoEMinus = rhoEMinus m
                                ,rhoEPlus = rhoEPlus m}}

prop_rhoL0Total e c@(Cell {mat = m}) sig = opLepton c' e sig == Opacity 0.0
  where c' = c { mat = Material{sig_abs = sig_abs m
                                ,sig_scat = sig_scat m
                                ,Material.vel = Material.vel m
                                ,tempE = tempE m
                                ,rhoNucl = rhoNucl m 
                                ,rhoEMinus = Density 0.0
                                ,rhoEPlus = Density 0.0}}

prop_rhoL0EMinus e c@(Cell {mat = m}) sig = opEMinus c' e sig == Opacity 0.0
  where c' = c { mat = Material{sig_abs = sig_abs m
                                ,sig_scat = sig_scat m
                                ,Material.vel = Material.vel m
                                ,tempE = tempE m
                                ,rhoNucl = rhoNucl m 
                                ,rhoEMinus = Density 0.0
                                ,rhoEPlus = rhoEPlus m}}

prop_rhoL0EPlus e c@(Cell {mat = m}) sig = opEPlus c' e sig == Opacity 0.0
  where c' = c { mat = Material{sig_abs = sig_abs m
                                ,sig_scat = sig_scat m
                                ,Material.vel = Material.vel m
                                ,tempE = tempE m
                                ,rhoNucl = rhoNucl m 
                                ,rhoEMinus = rhoEMinus m
                                ,rhoEPlus = Density 0.0}}

-- opacities should always be greater than 0
-- This relies on the energies and densities being greater than 0:
-- this is accomplished in declaring Arbitrary instances. 
prop_opCollide_ge0 c e sig = (mu $ opCollide c e sig) >= 0.0

prop_opN_ge0 c e = (mu $ opN c e ) >= 0.0

prop_opNAbs_ge0 c e = (mu $ opNAbs c e ) >= 0.0

prop_opNElastic_ge0 c e = (mu $ opNElastic c e ) >= 0.0

prop_opLepton_ge0 c e sig = (mu $ opLepton c e sig ) >= 0.0

prop_opEMinus_ge0 c e sig = (mu $ opEMinus c e sig ) >= 0.0

prop_opEPlus_ge0 c e sig = (mu $ opEPlus c e sig ) >= 0.0

tests = [testGroup "Opacity 0 when corresponding density is 0"
         [
           testProperty  "rho_nucl = 0 ==> opacity N Total -> 0" prop_rhoN0Total
         , testProperty  "rho_nucl = 0 ==> opacity N Absorb -> 0" prop_rhoN0Abs
         , testProperty  "rho_nucl = 0 ==> opacity N Elastic -> 0" prop_rhoN0Elastic
         , testProperty  "rho_lept = 0 ==> opacity Lep Total -> 0" prop_rhoL0Total
         , testProperty  "rho_e_minus = 0 ==> opacity e minus -> 0" prop_rhoL0EMinus
         , testProperty  "rho_e_plus = 0 ==> opacity e plus -> 0" prop_rhoL0EPlus
         ] 
        , testGroup "Opacities >= 0"
         [
           testProperty "opCollide >= 0" prop_opCollide_ge0
         , testProperty "opNTotal >= 0" prop_opN_ge0
         , testProperty "opNAbs >= 0" prop_opNAbs_ge0
         , testProperty "opNElastic >= 0" prop_opNElastic_ge0
         , testProperty "opLepton >= 0" prop_opLepton_ge0
         , testProperty "opEMinus >= 0" prop_opEMinus_ge0
         , testProperty "opEPlus >= 0" prop_opEPlus_ge0
          ]

        ]

-- End of file
