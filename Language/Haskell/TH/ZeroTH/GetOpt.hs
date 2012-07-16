{-# LANGUAGE TemplateHaskell, CPP, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-missing-signatures -pgmP cpphs -optP --hashes -optP --cpp #-}
module Language.Haskell.TH.ZeroTH.GetOpt where


import Control.Applicative                ( (<$>) )
import Data.List                          ( isPrefixOf )
import Data.Maybe                         ( fromMaybe )
import Data.Monoid.Record                 ( addP )
import System.Console.GetOpt              ( ArgDescr (..), OptDescr (..) )
import System.Console.GetOpt.Skeleton     ( mParseArgs )
import System.Directory                   ( findExecutable )
import System.Info                        ( os )
#ifdef version
import Distribution.Version               ( Version(..) )
#else
#define version undefined
#endif
import Language.Haskell.TH.ZeroTH.Config ( Config(..) )

import Language.Haskell.TH



import Data.Monoid                        ( Any(..), Last(..), Monoid(..) )
import System.Console.GetOpt.StandardOpts ( StandardFlag, stdOpts)

import Data.DeriveTH       ( derive )
import Data.Derive.LazySet ( makeLazySet )
import Data.Derive.Monoid  ( makeMonoid )



getExecutable :: String -> Maybe FilePath -> IO FilePath
getExecutable _ (Just path) = return path
getExecutable name Nothing  = fromMaybe (error errMsg) <$> findExecutable name
    where errMsg = "Couldn't find: "++name

mkConfig :: TempFlags -> IO Config
mkConfig tmpFlags
    = do ghcPath'   <- getExecutable "ghc" . getLast $ tempGHCPath tmpFlags
         cpphsPath' <- getExecutable "cpphs" . getLast $ tempCpphsPath tmpFlags
         return Config
                 { ghcPath    = ghcPath'
                 , cpphsPath  = cpphsPath'
                 , inputFile  = fromMaybe "-" . getLast $ tempInputFile tmpFlags
                 , outputFile = fromMaybe "-" . getLast $ tempOutputFile tmpFlags
                 , ghcArgs    = tempGHCArgs tmpFlags `orElse` defaultGhcArgs
                 , cpphsArgs  = tempCpphsArgs tmpFlags
                 , dropImport = tempDropImport tmpFlags `orElse` defaultDrop
                 , wholeFile  = not . getAny $ tempJustSplices tmpFlags}
    where
        defaultGhcArgs = ["-fno-code", "-o", nullFile, "-ohi", nullFile]
        nullFile
            | "mingw" `isPrefixOf` os = "NUL:"
            | otherwise               = "/dev/null"
        defaultDrop = ["Language.Haskell.TH"]

orElse :: [a] -> [a] -> [a]
orElse [] theDefault = theDefault
orElse x  _          = x



data TempFlags
    = TempFlags
    { tempGHCPath     :: Last FilePath
    , tempInputFile   :: Last FilePath
    , tempOutputFile  :: Last FilePath
    , tempCpphsPath   :: Last FilePath
    , tempGHCArgs     :: [String]
    , tempCpphsArgs   :: [String]
    , tempDropImport  :: [String]
    , tempJustSplices :: Any
    , tempStdFlag     :: Last StandardFlag
    }

derive makeMonoid ''TempFlags
derive makeLazySet ''TempFlags

-- XXX: Use Data.Derive to generate these instead
#define ADDER(FIELD,SET) FIELD ## ' = addP FIELD SET
#define ADDERT(FIELD) ADDER(temp ## FIELD, setTemp ## FIELD)
ADDERT(GHCPath)
ADDERT(InputFile)
ADDERT(OutputFile)
ADDERT(CpphsPath)
ADDERT(GHCArgs)
ADDERT(CpphsArgs)
ADDERT(DropImport)
ADDERT(JustSplices)
ADDERT(StdFlag)

globalOptions :: [OptDescr (TempFlags -> TempFlags)]
globalOptions = stdOpts tempStdFlag'
    ++ [ Option "" ["only-splices"] (NoArg $ tempJustSplices' True) "Only pass the splices to GHC, not the whole file (for faster processing)"
       , Option "w" ["ghc"] (ReqArg tempGHCPath' "PATH") "Use this GHC"
       , Option "" ["cpphs"] (ReqArg tempCpphsPath' "PATH") "Use this cpphs"
       , Option "i" ["input"] (ReqArg tempInputFile' "PATH") "Input file"
       , Option "o" ["output"] (ReqArg tempOutputFile' "PATH") "Output file"
       , Option "" ["ghc-args"] (ReqArg (tempGHCArgs' . words) "Arguments") "Arguments to GHC"
       , Option "" ["cpphs-args"] (ReqArg (tempCpphsArgs' . words) "Arguments") "Arguments to cpphs"
       , Option "d" ["drop-import"] (ReqArg (tempDropImport' . words) "Prefix") "Any import that starts with this prefix will be removed"
       ]

myParseArgs :: [String] -> IO TempFlags
myParseArgs = mParseArgs version globalOptions tempStdFlag
