-- | This module generates a Copilot Spec using a subset of the
-- copilot Language. More specifically, instead of generating all
-- types of streams (the ones with Typed instances) only generates
-- streams of type Word64 and Bool. This is since type classes is not
-- fully implemented in QuickGen yet but it is still possible to test
-- it using a limited number of specialized functions. Furthermore,
-- `drop' from the API have been left out since there are some
-- restrictions on it's usage.
module Main where

import Control.Exception.Base
import Control.Monad (when)
import Data.List
import GHC hiding (Type)
import GHC.Paths (libdir)
import System.Environment (getArgs)
import System.IO
import System.Random
import Testing.QuickGen
import Unsafe.Coerce

import Language

type CopilotName = String
-- | A Copilot expression represented as a Quickgen Exp and Type
type CopilotExpr = (Exp, Type)
type CopilotStream = (CopilotName, CopilotExpr)
type CopilotTrigger = (CopilotName, Exp, [CopilotExpr])
type CopilotSpec = ([CopilotStream], [CopilotTrigger])

someStreamTy :: Type
someStreamTy = Type [u] [] (ConT (mkName "Stream") [VarT u])
  where u = (0, Undecided)

boolStreamTy :: Type
boolStreamTy = Type [] [] (ConT (mkName "Stream") [ConT (mkName "Bool") []])

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

genTriggers :: Language -> Int -> (Int, Int) -> StdGen -> [CopilotTrigger]
genTriggers l n argsRange = go (map (('f':) . show) [1..n])
  where
    go []        _ = []
    go (name:ns) g =
        let ((guardExp, _), g') = genExpr l boolStreamTy g
            (numArgs, g'')      = randomR argsRange g'
            (args, g''')        = genArgs g'' numArgs []
            genArgs gen 0 acc   = (acc, gen)
            genArgs gen m acc   =
                let (r, gen') = genExpr l someStreamTy gen
                in genArgs gen' (m-1) (r : acc)
        in (name, guardExp, args) : go ns g'''

genSpec :: IO CopilotSpec
genSpec = do
    numStreams  <- randomRIO (2,12)
    numTriggers <- randomRIO (1,6)
    g <- getStdGen
    let (l', streams, g') = genStreams numStreams g
        triggers = genTriggers l' numTriggers (1,5) g'
    return (streams, triggers)

-- The code below is only for compilation of the generated value.
main :: IO ()
main = do
    hSetBuffering stdin LineBuffering

    (streams, triggers) <- genSpec

    let showExpr (e, t) = show e ++ " :: " ++ show t
        showArg e = "arg (" ++ showExpr e ++ ")"
        showTrigger (n, g, as) =
            concat [ "trigger " ++ show n ++ " (" ++ showExpr (g, boolStreamTy) ++ ") [ "
                   , intercalate ", " (map showArg as)
                   , " ]"
                   ]
        copilotString = concat [ "let {"
                               , intercalate ";\n     "
                                   [ n ++ " = " ++ showExpr e
                                   | (n, e) <- streams
                                   ]
                               , "\n    } in prettyPrint $ do { "
                               , intercalate (";\n" ++ replicate 28 ' ')
                                             (map showTrigger triggers)
                               , " }"
                               ]
        go = runGhc (Just libdir) $ do
            _ <- getSessionDynFlags >>= setSessionDynFlags
            addTarget =<< guessTarget "API.hs" Nothing
            load LoadAllTargets
            let prelude = (simpleImportDecl (mkModuleName "Prelude"))
                              { ideclQualified = True }
            setContext [ IIDecl prelude
                       , IIDecl . simpleImportDecl . mkModuleName $ "API"
                       , IIDecl . simpleImportDecl . mkModuleName $ "Copilot.Language"
                       ]
            r <- compileExpr copilotString
            r `seq` return (Right r)

    args <- getArgs
    when ("-v" `elem` args) $ do
        putStrLn "##################################################"
        putStrLn "### Haskell expression:"
        putStrLn $ copilotString
        putStrLn "##################################################\n"

    res <- catch go $ \e -> return (Left (e :: SomeException))
    case res of
        Left  exception -> error $ "This should not happen! " ++ show exception
        Right expr      -> (unsafeCoerce expr :: IO ())
