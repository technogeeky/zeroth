module Main ( main ) where

import Language.Haskell.TH.ZeroTH.GetOpt ( myParseArgs, mkConfig )
import Language.Haskell.TH.ZeroTH        ( zeroTH )

import System.Environment  ( getArgs )

main :: IO ()
main = zeroTH =<< mkConfig =<< myParseArgs =<< getArgs
