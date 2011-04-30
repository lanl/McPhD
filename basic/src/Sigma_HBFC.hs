{- | Analytic microscopic neutrino cross sections.
 - From Herant, Benz, Fryer, and Colgate: Ap. J. 435:339-361 (1994).
 - Cross sections are in cm^2, energies in MeV
 -}

module Sigma_HBFC where

import Physical


-- * neutrino--baryon events
-- ------------------------
-- | nucleon absorption/emission

nu_N_abs :: Energy -> CrossSection
nu_N_abs (Energy e) = CrossSection $ 9e-44 * e*e

-- | nu--nucleon elastic scattering
nu_N_elastic :: Energy -> CrossSection
nu_N_elastic (Energy e) = CrossSection $ 1.7e-44*e*e

-- -- | nu-nucleus elastic scatter: attempts to account
-- -- | for coherent scattering from bound nucleons
-- nu_N_Z_elastic
-- nu_N_Z_elastic (Energy e) N Z =
--   CrossSection $ 1.7e-44*(N-0.08*Z)*(N-0.08*Z)/6.0*e*e

-- | if not too concerned about the balance between n's & p's:
nu_A_elastic :: Energy -> NucleonNumber -> CrossSection
nu_A_elastic (Energy e) (NucleonNumber a) = CrossSection $ 1.7e-44*a*a/6.0*e*e


-- NOTE: the lepton cross sections are complicated by dependence on
-- species. The first set of functions is how I coded this in the
-- Python version. The corresponding opacity code figures out which
-- species is involved, then branches to one of these functions....

-- * neutrino--lepton scattering
-- ----------------------------
-- | nu_e--e^- & nu_bar_e--e^+ scattering
nu_e_e_minus :: Energy -> Energy -> CrossSection
nu_e_e_minus  (Energy e_nu) (Energy e_e) = CrossSection $ 9.2e-45 * e_nu * e_e

-- | nu_bar_e--e^- & nu_e--e^+ scattering
nu_e_e_plus :: Energy -> Energy -> CrossSection
nu_e_e_plus (Energy e_nu) (Energy e_e) = CrossSection $ 3.9e-45 * e_nu * e_e


-- | nu_x--e^- & nu_bar_x--e^+ scattering
nu_x_e_minus :: Energy -> Energy -> CrossSection
nu_x_e_minus  (Energy e_nu) (Energy e_e) = CrossSection $ 1.8e-45 * e_nu * e_e

-- | nu_bar_x--e^- & nu_x--e^+ scattering
nu_x_e_plus :: Energy -> Energy -> CrossSection
nu_x_e_plus (Energy e_nu) (Energy e_e) = CrossSection $ 1.3e-45 * e_nu * e_e



-- ...QUESTION: I'm hoping this will be more efficient (it's easier to code)
-- that what I have above.
-- The use case is something like this: we'll pass an instance of
-- Sigma.Lepton to MC.runParticle (or maybe curried into a MC.runParticleWith)
-- for a container of particles of one species. My hope is that
-- GHC will be able to determine statically which function is being called,
-- and inline it, or at least avoid the penalty of a function pointer.
-- Does that sound realistic?

-- | Lepton packs cross-section functions for each different neutrino species.
-- For the moment, we don't differentiate between tau and mu neutrinos (nu_x).
data Lepton = Lepton {
    e_minus :: Energy -> Energy -> CrossSection
  , e_plus  :: Energy -> Energy -> CrossSection
}

nu_e = Lepton {
    e_minus = nu_e_e_minus
  , e_plus  = nu_e_e_plus
}

nu_e_bar = Lepton {
    e_minus = nu_e_e_plus
  , e_plus  = nu_e_e_minus
}

nu_x = Lepton {
    e_minus = nu_x_e_minus
  , e_plus  = nu_x_e_plus
}

nu_x_bar = Lepton {
    e_minus = nu_x_e_plus
  , e_plus  = nu_x_e_minus
}

