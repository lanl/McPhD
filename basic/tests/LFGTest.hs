import qualified System.Random.MWC as MWC
import System.Random.LFG (defaultLags, largeLag, smallLag)
import System.Random.LFG.Pure as LFG
import Control.Monad

main =
  do
    mwc <- MWC.create
    initials <- replicateM (3 * largeLag defaultLags) (MWC.uniform mwc)
    print $ sum initials
    print $ smallLag defaultLags
    let rngs = LFG.sequences defaultLags initials
    print $ (rngs !! 2) !! 417

