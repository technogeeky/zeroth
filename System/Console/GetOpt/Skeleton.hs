{-# OPTIONS_GHC -fno-warn-orphans #-}
module System.Console.GetOpt.Skeleton where

import Data.Monoid                        ( Last(..), Monoid(..) )
import Distribution.Version               ( Version )
import System.Console.GetOpt              ( OptDescr (..), getOpt', ArgOrder (..) )
import System.Console.GetOpt.StandardOpts ( printHelp, printVersion, StandardFlag(..) )
import System.Environment                 ( getProgName )
import System.Exit                        ( exitWith, ExitCode (..) )
import Data.Maybe                         ( fromJust, isJust )

-- | *basic* command-line argument parsing - you may wish to write your own parseArgs, based on
-- the source code of this one.
parseArgs :: Version -> [OptDescr a] -> ([a] -> b) -> (b -> Maybe StandardFlag) -> [String] -> IO b
parseArgs progVersion globalOptions postProcess findStandard args =
  do
      progName <- getProgName
      let pHelp = printHelp progName globalOptions
      case getOpt' RequireOrder globalOptions args of
          (flags, _, _, []) | isJust (findStandard $ postProcess flags) -> do
              case fromJust (findStandard $ postProcess flags) of
                  HelpFlag -> pHelp
                  VersionFlag -> printVersion progName progVersion
              exitWith ExitSuccess
          (flags, [], _, []) -> return $ postProcess flags
          (_, _, _, errs) -> do putStrLn "Errors when parsing command-line arguments:"
                                mapM_ putStrLn errs
                                pHelp
                                exitWith $ ExitFailure 1

-- | Special version of parseArgs for monoids
mParseArgs :: Monoid b => Version -> [OptDescr (b -> b)] -> (b -> Last StandardFlag) -> [String] -> IO b
mParseArgs progVersion globalOptions getStandard = parseArgs progVersion globalOptions (foldr id mempty) $ getLast . getStandard

