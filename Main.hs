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
import Data.Functor
import Data.List
import GHC hiding (Type)
import GHC.Paths (libdir)
import System.Environment (getArgs)
import System.IO
import Unsafe.Coerce
import System.Random

import Generate

-- The code below is only for compilation of the generated value.
main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    (streams, triggers) <- fst . genSpec <$> getStdGen

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
