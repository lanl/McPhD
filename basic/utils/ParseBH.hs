-- ParseBH.hs
-- T. M. Kelley
-- Mar 20, 2012
-- (c) Copyright 2012 LANSLLC, all rights reserved

-- | Parse the output of the black-hole app into a Summary object. Everything
-- specific to black hole should be in this module.

module ParseBH 
  ( Summary(..)
  , stanzas
  , summSet
  , readSet
  , nuSumms
  , Nu(..)
  , FileSet(..)
  , IntTarget(..)
  , itargetVal
  , intTargets
  , DblTarget(..)
  , dtargetVal
  , dblTargets
  )
  where

import Control.Applicative ( (<$>),(<*), (*>), (<*>) )
import Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS

newtype FileSet = FileSet [FilePath]   deriving Show

type BStr = BS.ByteString

data IntTarget = IntTarget String deriving Show
data DblTarget = DblTarget String deriving Show

data Nu = NuE | NuEBar | NuX deriving Show

data Summary = 
  Summary { tsNP :: Int
            , tsTotalEEmit :: Double
            , tsMeanEW     :: Double
            -- bracketed doubles
            , tsTotEDep    :: Double
            , tsTotRMomDep :: Double
            , tsTotPL      :: Double
            -- scatters
            , tsNNuclEl    :: Int
            , tsNEMinus    :: Int
            , tsNEPlus     :: Int
            -- abs
            , tsNAbs    :: Int
            -- mesh
            , tsCBXing  :: Int
            , tsRefl    :: Int
            , tsEsc     :: Int
            -- timeouts
            , tsTO      :: Int
            -- total
            , tsMCSteps :: Int
            } deriving Show

stanzas s = map (summStanza s) [NuE,NuEBar,NuX]

summSet :: [BStr] -> [[Summary]]
summSet ss = map stanzas ss

readSet :: FileSet -> IO [BStr]
readSet (FileSet fs) = mapM BS.readFile fs

-- transpose to list per species
nuSumms :: Nu -> [[Summary]] -> [Summary]
nuSumms NuE    tss = map (\[a,_,_] -> a) tss
nuSumms NuEBar tss = map (\[_,b,_] -> b) tss
nuSumms NuX    tss = map (\[_,_,c] -> c) tss

-- parse one stanza into a Summary object
stanza = Summary <$> 
         
         ptclEmitP   <*>
         totalEEmitP <*>
         meanEWP     <*>
         totalEDepP  <*>
         totalRMomP  <*>
         totalPLP      <* (string $ BS.pack "Scatters:\n") <*>
         nuclElScatP <*>
         eMinusScatP <*>
         ePlusScatP    <* (string $ BS.pack "Absorptions:\n") <*>
         nuclAbsP      <* (string $ BS.pack "Mesh:\n") <*>
         bdyCrossP   <*>
         reflP       <*>
         escapesP      <* (string $ BS.pack "Timeouts:\n") <*>
         timeoutsP   <*>
         totalMCSP

-- parsers for individual lines
ptclEmitP,nuclElScatP,eMinusScatP,ePlusScatP,totalMCSP,
  escapesP, timeoutsP, reflP, bdyCrossP, nuclAbsP :: Parser Int
ptclEmitP   = (valKeyInt $ BS.pack "particles") <?> "particles emitted"
escapesP    = mkKeyValInt "escapes" "escapes"
timeoutsP   = mkKeyValInt "timeouts" "timeouts"
reflP       = mkKeyValInt "reflections" "reflections"
nuclElScatP = mkKeyValInt "nucleon elastic" "nucleon el scatters"
eMinusScatP = mkKeyValInt "electron scatters" "e- scatters"
ePlusScatP  = mkKeyValInt "positron scatters" "e+ scatters"
bdyCrossP   = mkKeyValInt "cell boundary crossings" "bdy crossings"
nuclAbsP    = mkKeyValInt "nu_i nucleon absorptions" "nucleon absorptions"
totalMCSP   = mkKeyValInt "Total number of MC steps" "total MC steps"

totalEEmitP, meanEWP, totalEDepP, totalRMomP, totalPLP :: Parser Double
totalEEmitP = valKeyDbl (BS.pack "total energy") <?> "energy emitted"
meanEWP     = valKeyDbl (BS.pack "mean energy weight") <?> "energy wt"

totalEDepP = bracketDbl "Total energy deposited: Energy " 
             "e" "total energy dep"
totalRMomP = bracketDbl "Net radial momentum deposited: Momentum "
             "mom" "net radial momentum dep"
totalPLP   = bracketDbl "Total path length: Distance "
             "distance" "total PL"

-- useful patterns
-- 's1 {s2 = d}'
bracketDbl s1 s2 s3 = lbrac s1 s2 *> double <* rbrac <* endOfLine <?> s3
  where rbrac = string (BS.pack "}")
        lbrac s1 s2= string $ (BS.pack s1) `BS.append` BS.pack "{" `BS.append`
                (BS.pack s2) `BS.append` BS.pack " = "
-- 's: i'
keyValInt, valKeyInt :: BStr -> Parser Int
keyValInt s = skipSpace *> key *> skipSpace *> decimal <* endOfLine
  where key = string $ s `BS.append` BS.pack ":"
mkKeyValInt key desc = (keyValInt $ BS.pack key) <?> desc
-- ' i s'
valKeyInt s = skipSpace *> decimal <* skipSpace <* string s <* endOfLine
-- ' d s' 
valKeyDbl :: BStr -> Parser Double
valKeyDbl s = skipSpace *> double <* skipSpace <* string s <* endOfLine

nuToString NuE    = "NuE"
nuToString NuEBar = "NuEBar"
nuToString NuX    = "NuX"

-- find the lines associated with one stanza of output,
-- indicated by a Nu instance
findLines :: BStr -> Nu -> BStr
findLines input nu = 
  fst . BS.breakSubstring tgtBot $ interesting
    where bottomChunk = snd $ BS.breakSubstring tgtTop input
          interesting = BS.unlines . tail . BS.lines $ bottomChunk
          tgtTop = BS.pack ("For " ++ nuToString nu)
          tgtBot = BS.pack ("===")

-- For each neutrino species in each file, form a Summary
summStanza :: BStr -> Nu -> Summary
summStanza s nu = 
  case parse stanza $ findLines s nu of
    Done _ ts -> ts
    _ -> error $ "summStanza: parse failed on species " ++ show nu


-- targets, plus binding targets to values
intTargets = map IntTarget [
              "ps emitted"
             ,"nucl el scats"
             ,"e-minus scats"
             ,"e-plus scats"
             ,"nucl abs"
             ,"bdy crossings"
             ,"reflections"
             ,"escapes"
             ,"timeouts"
             ,"MC steps"
             ]
itargetVal (IntTarget t) s = 
  case t of
    "ps emitted"     -> tsNP s
    "nucl el scats"  -> tsNNuclEl s
    "e-minus scats"  -> tsNEMinus s
    "e-plus scats"   -> tsNEPlus s
    "nucl abs"       -> tsNAbs s
    "bdy crossings" -> tsCBXing s
    "reflections"    -> tsRefl s
    "escapes"        -> tsEsc s
    "timeouts"       -> tsTO s
    "MC steps"       -> tsMCSteps s
    _ -> error $ "itargetVal: unrecognized target " ++ show t

dblTargets = map DblTarget [
              "e emitted"
             ,"mean ew"
             ,"e dep"
             ,"mom dep"
             ,"path length"
             ]
dtargetVal (DblTarget t) s =
  case t of 
    "e emitted"   -> tsTotalEEmit s
    "mean ew"     -> tsMeanEW s
    "e dep"       -> tsTotEDep s
    "mom dep"     -> tsTotRMomDep s
    "path length" -> tsTotPL s
    _ -> error $ "dtargetVal: unrecognized target " ++ show t

-- version
-- $Id$

-- End of file
