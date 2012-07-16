{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC
  -Wall -fno-warn-missing-signatures -pgmP cpphs -optP --hashes -optP --cpp 
  #-}
module Language.Haskell.TH.ZeroTH.GetOpt where
import Control.Applicative ((<$>))
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid.Record (addP)
import System.Console.GetOpt (ArgDescr(..), OptDescr(..))
import System.Console.GetOpt.Skeleton (mParseArgs)
import System.Directory (findExecutable)
import System.Info (os)
import Distribution.Version (Version(..))
import Language.Haskell.TH.ZeroTH.Config (Config(..))
import Language.Haskell.TH
import Data.Monoid (Any(..), Last(..), Monoid(..))
import System.Console.GetOpt.StandardOpts (StandardFlag, stdOpts)
 
getExecutable :: String -> Maybe FilePath -> IO FilePath
getExecutable _ (Just path) = return path
getExecutable name Nothing
  = fromMaybe (error errMsg) <$> findExecutable name
  where errMsg = "Couldn't find: " ++ name
 
mkConfig :: TempFlags -> IO Config
mkConfig tmpFlags
  = do ghcPath' <- getExecutable "ghc" . getLast $
                     tempGHCPath tmpFlags
       cpphsPath' <- getExecutable "cpphs" . getLast $
                       tempCpphsPath tmpFlags
       return
         Config{ghcPath = ghcPath', cpphsPath = cpphsPath',
                inputFile = fromMaybe "-" . getLast $ tempInputFile tmpFlags,
                outputFile = fromMaybe "-" . getLast $ tempOutputFile tmpFlags,
                ghcArgs = tempGHCArgs tmpFlags `orElse` defaultGhcArgs,
                cpphsArgs = tempCpphsArgs tmpFlags,
                dropImport = tempDropImport tmpFlags `orElse` defaultDrop,
                wholeFile = not . getAny $ tempJustSplices tmpFlags}
  where defaultGhcArgs
          = ["-fno-code", "-o", nullFile, "-ohi", nullFile]
        nullFile
          | "mingw" `isPrefixOf` os = "NUL:"
          | otherwise = "/dev/null"
        defaultDrop = ["Language.Haskell.TH"]
 
orElse :: [a] -> [a] -> [a]
orElse [] theDefault = theDefault
orElse x _ = x
