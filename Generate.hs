{-# LANGUAGE TemplateHaskell #-}

module Generate where

import Language (lang)

import System.Random
import Testing.QuickGen
import Copilot.Language

type CopilotName = String
-- | A Copilot expression represented as a Quickgen Exp and Type
type CopilotExpr = (Exp, Type)
type CopilotStream = (CopilotName, CopilotExpr)
type CopilotTrigger = (CopilotName, Exp, [CopilotExpr])
type CopilotSpec = ([CopilotStream], [CopilotTrigger])

someStreamTy :: Type
-- Cannot specify undecided types with getType unfortunately
someStreamTy = Type [u] [] (ConT (mkName "Stream") [VarT u])
  where u = (-1, Undecided)

boolStreamTy :: Type
boolStreamTy = $(getType [t| Stream Bool |])

genExpr :: Language -> Type -> StdGen -> (CopilotExpr, StdGen)
genExpr l t g = case generate l t seed of
    Nothing -> genExpr l t g'
    Just r  -> (r, g')
  where (seed, g') = next g

genStreams :: Int -> StdGen -> (Language, [CopilotStream], StdGen)
genStreams n = go lang (map (('s':) . show) [1..n]) []
  where
    go l []     acc g = (l, reverse acc, g)
    go l (name:ns) acc g =
        let (r@(_, ty), g') = genExpr l someStreamTy g
            c    = (mkName name, ty)
            l'   = [c] `addTo` l
        in go l' ns ((name, r) : acc) g'

genTriggers :: Language -> Int -> (Int, Int) -> StdGen -> ([CopilotTrigger], StdGen)
genTriggers l n argsRange = go (map (('f':) . show) [n,n-1..1]) []
  where
    go []        acc g = (acc, g)
    go (name:ns) acc g =
        let ((guardExp, _), g1) = genExpr l boolStreamTy g
            (numArgs, g2)       = randomR argsRange g1
            (args, g3)          = genArgs g2 numArgs []
            genArgs gen 0 acc'  = (acc', gen)
            genArgs gen m acc'  =
                let (r, gen') = genExpr l someStreamTy gen
                in genArgs gen' (m-1) (r : acc')
        in go ns ((name, guardExp, args) : acc) g3

genSpec :: StdGen -> (CopilotSpec, StdGen)
genSpec g1 = let (numStreams,  g2) = randomR (2,12) g1
                 (numTriggers, g3) = randomR (1,6) g2
                 (l, streams,  g4) = genStreams numStreams g3
                 (triggers,    g5) = genTriggers l numTriggers (1,5) g4
             in ((streams, triggers), g5)
