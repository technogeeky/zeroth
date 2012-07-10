{-# OPTIONS_GHC
  -Wall -fno-warn-missing-signatures -pgmP cpphs -optP --hashes -optP --cpp 
  #-}
module Language.Haskell.TH.ZeroTH.GetOpt where
import Control.Applicative ((<$>))
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid (Any(..), Last(..), Monoid(..))
import Data.Monoid.Record (addP)
import System.Console.GetOpt (ArgDescr(..), OptDescr(..))
import System.Console.GetOpt.Skeleton (mParseArgs)
import System.Console.GetOpt.StandardOpts (StandardFlag, stdOpts)
import System.Directory (findExecutable)
import System.Info (os)
import Distribution.Version (Version(..))
import Language.Haskell.TH.ZeroTH.Config (Config(..))

globalOptions :: [] (OptDescr (TempFlags -> TempFlags))
globalOptions
  = stdOpts tempStdFlag' ++
     [ Option ""  ["only-splices"] (NoArg $ tempJustSplices' True)                "Only pass the splices to GHC, not the whole file (for faster processing)"
     , Option "w" ["ghc"]          (ReqArg tempGHCPath' "PATH"   )                "Use this GHC"
     , Option ""  ["cpphs"]        (ReqArg tempCpphsPath' "PATH" )                "Use this cpphs"
     , Option "i" ["input"]        (ReqArg tempInputFile' "PATH" )                "Input file"
     , Option "o" ["output"]       (ReqArg tempOutputFile' "PATH")                "Output file"
     , Option ""  ["ghc-args"]     (ReqArg (tempGHCArgs'    . words) "Arguments") "Arguments to GHC"
     , Option ""  ["cpphs-args"]   (ReqArg (tempCpphsArgs'  . words) "Arguments") "Arguments to cpphs"
     , Option "d" ["drop-import"]  (ReqArg (tempDropImport' . words) "Prefix")    "Any import that starts with this prefix will be removed"
     ]


getExecutable :: String -> Maybe FilePath -> IO FilePath
getExecutable _ (Just path) = return path
getExecutable name (Nothing)
  = fromMaybe (error errMsg) <$> findExecutable name
  where errMsg = "Couldn't find: " ++ name
 
mkConfig :: TempFlags -> IO Config
mkConfig tmpFlags
  = do ghcPath' <- getExecutable "ghc" . getLast $
                     tempGHCPath tmpFlags
       cpphsPath' <- getExecutable "cpphs" . getLast $
                       tempCpphsPath tmpFlags
       return
         Config{ 
                 ghcPath      = ghcPath'
               , cpphsPath    = cpphsPath'
               , inputFile    = fromMaybe "-" . getLast $ tempInputFile tmpFlags
               , outputFile   = fromMaybe "-" . getLast $ tempOutputFile tmpFlags
               , ghcArgs      = tempGHCArgs tmpFlags `orElse` defaultGhcArgs
               , cpphsArgs    = tempCpphsArgs tmpFlags
               , dropImport   = tempDropImport tmpFlags `orElse` defaultDrop
               , wholeFile    = not . getAny $ tempJustSplices tmpFlags
                }
  where defaultGhcArgs
          = ["-fno-code"
            , "-o"
            , nullFile
            , "-ohi"
            , nullFile]
        nullFile
          | "mingw" `isPrefixOf` os = "NUL:"
          | otherwise = "/dev/null"
        defaultDrop = ["Language.Haskell.TH"]
 
orElse :: [] a -> [] a -> [] a
orElse [] theDefault = theDefault
orElse x _ = x
 
data TempFlags = TempFlags{
                             tempGHCPath     :: Last FilePath
                           , tempInputFile   :: Last FilePath
                           , tempOutputFile  :: Last FilePath
                           , tempCpphsPath   :: Last FilePath
                           , tempGHCArgs     :: [] String
                           , tempCpphsArgs   :: [] String
                           , tempDropImport  :: [] String
                           , tempJustSplices :: Any
                           , tempStdFlag     :: Last StandardFlag
                           }
 
instance Monoid TempFlags where
        mempty =      TempFlags mempty mempty mempty mempty mempty mempty mempty mempty mempty
        mappend      (TempFlags x1     x2     x3     x4     x5     x6     x7     x8     x9     )
                     (TempFlags y1     y2     y3     y4     y5     y6     y7     y8     y9     )
          = TempFlags 
              (mappend x1 y1) 
              (mappend x2 y2) 
              (mappend x3 y3)
              (mappend x4 y4)
              (mappend x5 y5)
              (mappend x6 y6)
              (mappend x7 y7)
              (mappend x8 y8)
              (mappend x9 y9)
setTempGHCPath a0 b0
  = TempFlags a0 (tempInputFile b0) (tempOutputFile b0)
      (tempCpphsPath b0)
      (tempGHCArgs b0)
      (tempCpphsArgs b0)
      (tempDropImport b0)
      (tempJustSplices b0)
      (tempStdFlag b0)
setTempInputFile a0 b0
  = TempFlags (tempGHCPath b0) a0 (tempOutputFile b0)
      (tempCpphsPath b0)
      (tempGHCArgs b0)
      (tempCpphsArgs b0)
      (tempDropImport b0)
      (tempJustSplices b0)
      (tempStdFlag b0)
setTempOutputFile a0 b0
  = TempFlags (tempGHCPath b0) (tempInputFile b0) a0
      (tempCpphsPath b0)
      (tempGHCArgs b0)
      (tempCpphsArgs b0)
      (tempDropImport b0)
      (tempJustSplices b0)
      (tempStdFlag b0)
setTempCpphsPath a0 b0
  = TempFlags (tempGHCPath b0) (tempInputFile b0) (tempOutputFile b0)
      a0
      (tempGHCArgs b0)
      (tempCpphsArgs b0)
      (tempDropImport b0)
      (tempJustSplices b0)
      (tempStdFlag b0)
setTempGHCArgs a0 b0
  = TempFlags (tempGHCPath b0) (tempInputFile b0) (tempOutputFile b0)
      (tempCpphsPath b0)
      a0
      (tempCpphsArgs b0)
      (tempDropImport b0)
      (tempJustSplices b0)
      (tempStdFlag b0)
setTempCpphsArgs a0 b0
  = TempFlags (tempGHCPath b0) (tempInputFile b0) (tempOutputFile b0)
      (tempCpphsPath b0)
      (tempGHCArgs b0)
      a0
      (tempDropImport b0)
      (tempJustSplices b0)
      (tempStdFlag b0)
setTempDropImport a0 b0
  = TempFlags (tempGHCPath b0) (tempInputFile b0) (tempOutputFile b0)
      (tempCpphsPath b0)
      (tempGHCArgs b0)
      (tempCpphsArgs b0)
      a0
      (tempJustSplices b0)
      (tempStdFlag b0)
setTempJustSplices a0 b0
  = TempFlags (tempGHCPath b0) (tempInputFile b0) (tempOutputFile b0)
      (tempCpphsPath b0)
      (tempGHCArgs b0)
      (tempCpphsArgs b0)
      (tempDropImport b0)
      a0
      (tempStdFlag b0)
setTempStdFlag a0 b0
  = TempFlags (tempGHCPath b0) (tempInputFile b0) (tempOutputFile b0)
      (tempCpphsPath b0)
      (tempGHCArgs b0)
      (tempCpphsArgs b0)
      (tempDropImport b0)
      (tempJustSplices b0)
      a0

--  XXX: Use Data.Derive to generate these instead
tempGHCPath'        = addP tempGHCPath       setTempGHCPath
tempInputFile'      = addP tempInputFile     setTempInputFile
tempOutputFile'     = addP tempOutputFile    setTempOutputFile
tempCpphsPath'      = addP tempCpphsPath     setTempCpphsPath
tempGHCArgs'        = addP tempGHCArgs       setTempGHCArgs
tempCpphsArgs'      = addP tempCpphsArgs     setTempCpphsArgs
tempDropImport'     = addP tempDropImport    setTempDropImport
tempJustSplices'    = addP tempJustSplices   setTempJustSplices
tempStdFlag'        = addP tempStdFlag       setTempStdFlag
 
 
myParseArgs :: [] String -> IO TempFlags
myParseArgs
  = mParseArgs
      Version{versionBranch = [2009, 6, 23, 3], versionTags = []}
      globalOptions
      tempStdFlag
