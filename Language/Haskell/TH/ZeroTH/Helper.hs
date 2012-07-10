module Language.Haskell.TH.ZeroTH.Helper ( helper, idPrefix ) where

import Control.Applicative        ( (<$>) )
import Data.Generics.Schemes      ( listify )
import Data.Maybe                 ( fromJust, isJust )
import Language.Haskell.TH.Ppr    ( pprint )
import Language.Haskell.TH.Syntax ( Dec, nameModule, Q, runIO )
import System.IO                  ( hFlush, stdout )

import Language.Haskell.TH.ZeroTH.Comments ( Location )

idPrefix :: String
idPrefix = "ZEROTH OUTPUT: "

helper :: Q [Dec] -> Location -> Q [Dec]
helper splice loc = do
    decls <- splice
    runIO $ do putStrLn $ idPrefix ++ show ((loc, unlines $ pprint <$> decls), map (fromJust . nameModule) $ listify (isJust . nameModule) decls)
               hFlush stdout
    return decls