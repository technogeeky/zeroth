module Distribution.ZeroTH where

import Data.Foldable                      ( fold )
import Data.Monoid                        ( mempty )
import Distribution.Package               ( PackageIdentifier(..) )
import Distribution.PackageDescription    ( PackageDescription(..) )
import Distribution.Simple                ( Args, defaultMainWithHooks, simpleUserHooks, postConf )
import Distribution.Simple.LocalBuildInfo ( LocalBuildInfo )
import Distribution.Simple.Setup          ( ConfigFlags(..) )
import System.Directory                   ( createDirectoryIfMissing )
import System.FilePath                    ( (</>), takeDirectory )

import Language.Haskell.TH.ZeroTH        ( zeroTH )
import Language.Haskell.TH.ZeroTH.GetOpt ( mkConfig, tempCpphsArgs', tempCpphsPath', tempDropImport', TempFlags, tempGHCArgs', tempGHCPath', tempInputFile', tempOutputFile' )

zeroTHCabalMain :: Maybe [String] -> [String] -> [FilePath] -> IO ()
zeroTHCabalMain dropIm extraCpphsArgs relevantFiles
    = defaultMainWithHooks $ simpleUserHooks { postConf = zeroTHPostConf dropIm extraCpphsArgs relevantFiles }

zeroTHPostConf :: Maybe [String] -> [String] -> [FilePath] -> Args -> ConfigFlags -> PackageDescription -> LocalBuildInfo -> IO ()
zeroTHPostConf dropIm extraCpphsArgs relevantFiles _
               (ConfigFlags { configProgramPaths = paths, configProgramArgs = args })
               (PackageDescription { package = PackageIdentifier { pkgVersion = version } })
               _ = let ppFlags = tempCpphsArgs' (("-Dversion=" ++ show version) : extraCpphsArgs ++ fold (lookup "cpphs" args))
                                 . maybe id tempCpphsPath' (lookup "cpphs" paths)
                                 . maybe id tempDropImport' dropIm
                                 . tempGHCArgs' ("--make" : fold (lookup "ghc" args))
                                 $ maybe id tempGHCPath' (lookup "ghc" paths) mempty
                       in mapM_ (ppZeroTH ppFlags) relevantFiles

-- | Run zeroTH as a preprocessor on the given file
ppZeroTH :: TempFlags -> FilePath -> IO ()
ppZeroTH flags inputFile = do
    let outputFile = ppOutputDir </> inputFile
    createDirectoryIfMissing True $ takeDirectory outputFile
    zeroTH =<< mkConfig (tempInputFile' inputFile $ tempOutputFile' outputFile flags)

-- | The directory in which generated files are written
ppOutputDir :: FilePath
ppOutputDir = "ppsrc"